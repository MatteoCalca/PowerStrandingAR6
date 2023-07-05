rm(list = ls())

library(reticulate)
library(tidyverse)
library(janitor)
library(readxl)
library(dtplyr)


# map the directory with the data - ADJUST THIS TO YOUR SYSTEM!
dir_database <- "C:/Users/pwaidelich/Downloads/IIASA Scenario Database"
list.files(dir_database)

# read in country-level results (based on downscaling)
df_iso3 <- read_csv(file.path(dir_database, "AR6_Scenarios_Database_ISO3_v1.1.csv")) %>%
  clean_names() %>% mutate(modelscenario = paste0(model, scenario))

# read in R10 results
df_r10 <- read_csv(file.path(dir_database, "AR6_Scenarios_Database_R10_regions_v1.1.csv")) %>%
  clean_names()

# read in global results
df_global <- read_csv(file.path(dir_database, "AR6_Scenarios_Database_World_v1.1.csv")) %>%
  clean_names()

# read in meta information from the IIASA Database
df_meta <-  read_excel(file.path(dir_database, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),
                       sheet = "meta_Ch3vetted_withclimate") %>%
  clean_names() %>% mutate(modelscenario = paste0(model, scenario))

# glimpse at the data
glimpse(df_iso3)
glimpse(df_r10)

# identify the variables of interest
unique(df_iso3$variable) %>% str_subset("[Cc]apacity")
unique(df_iso3$variable) %>% str_subset("[Cc]oal") %>% sort()
unique(df_iso3$variable) %>% str_subset("[Pp]rice") %>% sort()

# could differentiate w/ and w/o ccs for coal - for now, we focus on w/o CCS

# potential variables of interest:
c("Capacity Additions|Electricity|Coal",
  "Capacity|Electricity|Coal",
  "Cumulative Capacity|Electricity|Coal",
  "Investment|Energy Supply|Electricity|Coal",
  "Lifetime|Electricity|Coal|w/o CCS",
  "OM Cost|Fixed|Electricity|Coal|w/o CCS" ,
  "Price|Primary Energy|Coal" ,
  "Price|Secondary Energy|Solids|Coal",
  "Secondary Energy|Electricity|Coal"
  )

# create an India-specific subset
df_ind <- df_iso3 %>% filter(region == "IND") %>% mutate(modelscenario = paste0(model, scenario))

# check what key variables are available for which model-scenario pairing
df_variablecheck <- df_ind %>% group_by(model, scenario, modelscenario) %>%
  summarise(has_secondary = "Secondary Energy|Electricity|Coal" %in% variable,
            has_price = "Price|Primary Energy|Coal" %in% variable,
            has_capacity = "Capacity|Electricity|Coal" %in% variable,
            has_om = "OM Cost|Fixed|Electricity|Coal|w/o CCS" %in% variable,
            has_carbonprice = "Price|Carbon" %in% variable
            ) %>%
  mutate(is_vetted = modelscenario %in% df_meta$modelscenario)

# plot available number of scenarios per model
df_variablecheck %>% filter(has_secondary & has_price & has_capacity & has_om & has_carbonprice) %>%
  count(model, is_vetted) %>%
  ggplot(aes(reorder(model, n), n)) + geom_col(aes(fill = is_vetted)) + coord_flip() +
  labs(subtitle = "Model with coal price, generation, capacity, OM & carbon price", x = NULL, y = "# of model-scenario pairs in AR6 Database") +
  theme(legend.position = "bottom")

# define what model scenarios we use (due to data availabilibility & mapping)
modelscenario_ind_used <- df_variablecheck %>% filter(has_secondary & has_price & has_capacity & has_om & has_carbonprice & is_vetted) %>%
  pull(modelscenario)

# create the actual database by interpolating
df <- df_ind %>% filter(modelscenario %in% modelscenario_ind_used) %>%
  # merge in relevant categories from the metadata
  left_join(df_meta %>% select(modelscenario, category, policy_category), by = "modelscenario") %>%
  # subset to the variables of interest
  filter(variable %in% c("Secondary Energy|Electricity|Coal", "Price|Primary Energy|Coal", "Capacity|Electricity|Coal",
                         "OM Cost|Fixed|Electricity|Coal|w/o CCS", "Price|Carbon")) %>%
  # convert to long format (one row per model-scenario-year-variable pairing)
  pivot_longer(cols = starts_with("x"), values_to = "value", names_to = "year") %>%
  # clean up the year column
  mutate(year = str_remove(year, "^x") %>% as.integer())

# interpolate values for each year between 2010 and 2100
# METHOD FOR NOW: linear between data points, holding constant at the extremes (= rule 2 for na.approx)
df <- df %>% select(-year, -value) %>% distinct() %>%
  mutate(year = list(2010:2100)) %>% unnest(year) %>%
  left_join(df, by = c("model", "scenario", "region", "variable", "unit", "modelscenario", "category", "policy_category", "year")) %>%
  mutate(value_interpolated = zoo::na.approx(value, method = "linear", rule = 2))

# ensure that variable-unit pairings are unique
df %>% count(variable, unit) %>% print()

# create a data with one row per model-scenario-year and the relevant variables in columns
df <- df %>% select(-value, -modelscenario, -unit) %>%  pivot_wider(values_from = "value_interpolated", names_from = "variable") %>%
  clean_names()

# export this 
write_csv(df, file.path("data", "df_india.csv"))