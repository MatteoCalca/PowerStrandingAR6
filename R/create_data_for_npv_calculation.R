rm(list = ls())

library(reticulate)
library(tidyverse)
library(janitor)
library(readxl)
library(dtplyr)

# map the directory of the AR6 database files - ADJUST THIS TO YOUR SYSTEM
dir_database = "C:/Users/pwaidelich/Downloads/IIASA Scenario Database"

# read in country-level results (based on downscaling) and filter to region of interest
df_iso3 <- read_csv(file.path(dir_database, "AR6_Scenarios_Database_ISO3_v1.1.csv")) %>%
  clean_names() %>% mutate(modelscenario = paste0(model, scenario))

# read in meta information from the IIASA Database
df_meta <-  read_excel(file.path(dir_database, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),
                       sheet = "meta_Ch3vetted_withclimate") %>%
  clean_names() %>% mutate(modelscenario = paste0(model, scenario))

# write a function to calculate NPV
create_data_for_npv_calculation <- function(region_selected = NULL,
                                            df_ar6_iso3 = NULL,
                                            df_ar6_meta = NULL,
                                            variables_to_extract = c("Secondary Energy|Electricity|Coal",
                                                                     "Price|Primary Energy|Coal",
                                                                     "Capacity|Electricity|Coal",
                                                                     "OM Cost|Fixed|Electricity|Coal|w/o CCS",
                                                                     "Price|Carbon",
                                                                     "Primary Energy|Coal|Electricity",
                                                                     "Price|Secondary Energy|Electricity"),
                                            create_charts = T,
                                            return_output = T
) {
  
  # check what key variables are available for which model-scenario pairing
  df_variablecheck <- df_ar6_iso3 %>% filter(region == region_selected) %>% group_by(model, scenario, modelscenario, region) %>%
    summarise(has_generation = "Secondary Energy|Electricity|Coal" %in% variable,
              has_price = "Price|Primary Energy|Coal" %in% variable,
              has_capacity = "Capacity|Electricity|Coal" %in% variable,
              has_coaluse = "Primary Energy|Coal|Electricity" %in% variable,
              has_om = "OM Cost|Fixed|Electricity|Coal|w/o CCS" %in% variable,
              has_carbonprice = "Price|Carbon" %in% variable,
              has_electricityprice = "Price|Secondary Energy|Electricity" %in% variable,
              .groups = "drop") %>%
    mutate(is_vetted = modelscenario %in% df_ar6_meta$modelscenario)
  
  # define what model scenarios we use (due to data availabilibility & mapping)
  modelscenario_used <- df_variablecheck %>% filter(has_generation & has_price & has_capacity & has_coaluse & has_om & has_carbonprice & has_electricityprice & is_vetted) %>%
    pull(modelscenario)
  
  # exit prematurely if there is no
  if(length(modelscenario_used) == 0) {
    paste0("No model-scenario found with all variables of interest for region ", region_selected) %>% print()
    return(FALSE)
  }
  
  # otherwise, continue
  if(create_charts) {
    # plot available number of scenarios per model
    df_variablecheck %>% filter(has_generation & has_price & has_capacity & has_coaluse & has_om & has_carbonprice & has_electricityprice) %>%
      count(model, is_vetted) %>%
      ggplot(aes(reorder(model, n), n)) + geom_col(aes(fill = is_vetted)) + coord_flip() +
      labs(subtitle = "Model with coal price, generation, capacity, OM, electricity & carbon price", x = NULL, y = "# of model-scenario pairs in AR6 Database") +
      theme(legend.position = "bottom")
  }

 
  
  # create the actual database by interpolating
  df <- df_ar6_iso3 %>% filter(modelscenario %in% modelscenario_used, region == region_selected) %>%
    # merge in relevant categories from the metadata
    left_join(df_ar6_meta %>% select(modelscenario, category, policy_category), by = "modelscenario") %>%
    # subset to the variables of interest
    filter(variable %in% variables_to_extract) %>%
    # convert to long format (one row per model-scenario-year-variable pairing)
    pivot_longer(cols = starts_with("x"), values_to = "value", names_to = "year") %>%
    # clean up the year column
    mutate(year = str_remove(year, "^x") %>% as.integer())
  
  # check if we have unique variable-unit pairings across all model-scenarios left in the subset
  if(df %>% count(variable, unit) %>% nrow() != length(variables_to_extract)) stop("Multiple units per variable detected. Please inspect")
  # unit_dictionary <- df %>% select(variable, unit) %>% distinct()
  # saveRDS(unit_dictionary, file.path("data", "unit_dictionary.rds"))
  
  # interpolate values for each year between 2010 and 2100
  # METHOD FOR NOW: linear between data points, holding constant at the extremes (= rule 2 for na.approx)
  df <- df %>% select(-year, -value) %>% distinct() %>%
    mutate(year = list(2010:2100)) %>% unnest(year) %>%
    left_join(df, by = c("model", "scenario", "region", "variable", "unit", "modelscenario", "category", "policy_category", "year")) %>%
    mutate(value_interpolated = zoo::na.approx(value, method = "linear", rule = 2))
  
  # create a data with one row per model-scenario-year and the relevant variables in columns
  df <- df %>% select(-value, -modelscenario, -unit) %>%  pivot_wider(values_from = "value_interpolated", names_from = "variable") %>%
    clean_names()
  
  # add policy category in a coarse way based on regex detection
  df <- df %>% mutate(policy_category_coarse = case_when(str_detect(policy_category, "^P0") ~ "Diagnostics",
                                                         str_detect(policy_category, "^P1") ~ "Baseline",
                                                         str_detect(policy_category, "^P2") ~ "Immediate action",
                                                         str_detect(policy_category, "^P3") ~ "Delayed action",
                                                         TRUE ~ NA_character_
  ))
  
  # export the data frame as a CSV file
  write_csv(df, file.path("data", "output", "ar6_database_coal", paste0(region_selected, ".csv")))
  
  if(return_output) {
    return(df)
  } else {
    return(TRUE)
  }
}

# execute to test
# create_data_for_npv_calculation(region_selected = "IND", df_ar6_iso3 = df_iso3, df_ar6_meta = df_meta)

# do for all countries in the AR6 database - NOTE: this runs for several minutes
map(unique(df_iso3$region), ~ create_data_for_npv_calculation(region_selected = .x,
                                                              df_ar6_iso3 = df_iso3, 
                                                              df_ar6_meta = df_meta,
                                                              return_output = F,
                                                              create_charts = F))

