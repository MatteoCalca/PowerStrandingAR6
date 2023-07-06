rm(list = ls())

library(tidyverse)
library(ggpubr)
library(ggthemes)

calculate_npv_loss <- function(data = NULL,
                               model_selected = NULL,
                               scenario_baseline = "EN_INDCi2100",
                               scenario_delayed = "EN_INDCi2030_800f",
                               carbonintensity_g_per_kwh = 880,
                               kwh_per_exajoule = 277777777777.7778,
                               discount_rate = 0.05,
                               base_year = 2020,
                               write_out_annuals = T) {
  
  # check inputs
  if(is.null(data) | is.null(model_selected)) stop("Please provide arguments data and model_selected")
  
  # extract the region from the data argument supplied
  region <- unique(data$region)
  
  # exit prematurely if there is no data
  if(nrow(data %>% filter(scenario %in% c(scenario_baseline, scenario_delayed) & model == model_selected)) == 0) {
    paste("There is no data for region", region, "in the model", model_selected, ". Exiting prematurely") %>% print()
    return(NULL)
  }
  
  # subset the data and calculate revenues and cost
  df <- data %>% filter(scenario %in% c(scenario_baseline, scenario_delayed) & model == model_selected) %>%
    mutate(revenue = (10^9*price_secondary_energy_electricity)*secondary_energy_electricity_coal,
           capex = investment_energy_supply_electricity_coal,
           cost_fuel = (10^9*price_primary_energy_coal)*primary_energy_coal_electricity,
           cost_om = 10^6*om_cost_fixed_electricity_coal_w_o_ccs*capacity_electricity_coal,
           cost_carbonprice = 10^(-6)*carbonintensity_g_per_kwh*(secondary_energy_electricity_coal*kwh_per_exajoule)*price_carbon,
           opex = cost_fuel + cost_om + cost_carbonprice,
           profit_opexonly = revenue - opex,
           profit = revenue - opex - capex
    )
  
  # discount profits to base year
  df <- df %>% filter(year >= base_year) %>%
    mutate(time_from_baseyear = year - base_year,
           discount_factor = (1 + discount_rate)^(-time_from_baseyear),
           profit_pv = discount_factor*profit
    )
  
  # export the annual profits as CSV file
  if(write_out_annuals) write_csv(df, file.path("data", "output", "annual_profits_coal",
                                                paste0(region, "_", model_selected, "_", scenario_baseline, "_", scenario_delayed, ".csv")))
  
  # calculate the present value
  df_out <- df %>% group_by(model, scenario) %>%
    summarise(npv = sum(profit_pv), .groups = "drop") %>%
    pivot_wider(values_from = "npv", names_from = "scenario") %>%
    mutate(npv_change = !!as.name(scenario_delayed) -  !!as.name(scenario_baseline),
           npv_change_relative = npv_change/!!as.name(scenario_baseline),
           scenario_baseline = scenario_baseline,
           scenario_delayed = scenario_delayed,
           region = region,
           base_year = base_year,
           discount_rate = discount_rate,
           carbonintensity_g_per_kwh = carbonintensity_g_per_kwh) %>%
    rename(npv_baseline = all_of(scenario_baseline),
           npv_delayed = all_of(scenario_delayed)) %>%
    select(model, region, scenario_baseline, scenario_delayed,
           npv_baseline, npv_delayed, npv_change, everything())
  
  return(df_out)
}

# example
read_csv(file.path("data", "output", "ar6_database_coal", "IND.csv")) %>% calculate_npv_loss(model_selected = "REMIND-MAgPIE 2.1-4.2")

# 
# scenario_baseline = "EN_INDCi2100"
# scenario_delayed = "EN_INDCi2030_800f"
# carbonintensity_g_per_kwh = 880
# kwh_per_exajoule = 277777777777.7778
# discount_rate = 0.05
# base_year = 2020

iso3_codes_available <- file.path("data", "output", "ar6_database_coal") %>% list.files() %>% str_remove("\\.csv$") %>%
  str_subset("^R10|^R6", negate = T)

df_input <- crossing(model_selected = c("IMAGE 3.0", "POLES ENGAGE", "REMIND-MAgPIE 2.1-4.2"),
                     region = iso3_codes_available)

wrapper_function <- function(model_selected = NULL,
                             region = NULL) {
  read_csv(file.path("data", "output", "ar6_database_coal", paste0(region, ".csv"))) %>%
    calculate_npv_loss(model = model_selected)
}

# for(jj in 1:nrow(df_input)) {
#   paste(df_input$model_selected[jj], df_input$region[jj]) %>% print()
#   wrapper_function(df_input$model_selected[jj], df_input$region[jj])
# }

# run calculations for all selected models and countries, then write out results as a CSV
df <- pmap_dfr(df_input, wrapper_function)
write_csv(df, file.path("data", "output", "df_npv.csv"))