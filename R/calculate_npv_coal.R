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
                               create_charts = T) {
  
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
           cost_fuel = (10^9*price_primary_energy_coal)*primary_energy_coal_electricity,
           cost_om = 10^6*om_cost_fixed_electricity_coal_w_o_ccs*capacity_electricity_coal,
           cost_carbonprice = 10^(-6)*carbonintensity_g_per_kwh*(secondary_energy_electricity_coal*kwh_per_exajoule)*price_carbon,
           profit = revenue - cost_fuel - cost_om - cost_carbonprice
    )
  
  # plot how fundamentals used for calculation evolve over time
  if(create_charts) {
  ggarrange(df %>% ggplot(aes(year, price_carbon)) + geom_line(aes(colour = policy_category_coarse)) + scale_colour_wsj() + theme_wsj(),
            df %>% ggplot(aes(year, capacity_electricity_coal)) + geom_line(aes(colour = policy_category_coarse)) + scale_colour_wsj() + theme_wsj(),
            df %>% ggplot(aes(year, price_primary_energy_coal)) + geom_line(aes(colour = policy_category_coarse)) + scale_colour_wsj() + theme_wsj(),
            df %>% ggplot(aes(year, secondary_energy_electricity_coal)) + geom_line(aes(colour = policy_category_coarse)) + scale_colour_wsj() + theme_wsj(),
            df %>% ggplot(aes(year, om_cost_fixed_electricity_coal_w_o_ccs)) + geom_line(aes(colour = policy_category_coarse)) + scale_colour_wsj() + theme_wsj(),
            nrow = 2, ncol = 3, common.legend = T, legend = "bottom"
  )
    ggsave(file.path("figures", paste0(region, "_", model_selected, "_", scenario_baseline, "_", scenario_delayed, "_fundamentals.png")), width = 10, height = 7)
  }
  
  if(create_charts) {
    ggarrange(
      df %>% pivot_longer(cols = c(revenue, cost_fuel, cost_om, cost_carbonprice, profit)) %>%
        mutate(value_per_exajoule = value / secondary_energy_electricity_coal) %>%
        ggplot(aes(year, value_per_exajoule/10^9)) + geom_line(aes(colour = name)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
        labs(y = "USD bn/EJ", colour = NULL) + facet_wrap(~ scenario, scales = "free_y") +
        theme_wsj(),
      
      df %>% pivot_longer(cols = c(revenue, cost_fuel, cost_om, cost_carbonprice, profit)) %>%
        ggplot(aes(year, value/10^9)) + geom_line(aes(colour = name)) +
        geom_hline(yintercept = 0, linetype = "dotted", colour = "black") +
        labs(y = "USD bn", colour = NULL) + facet_wrap(~ scenario) +
        theme_wsj(),
      nrow = 2, ncol = 1, common.legend = T, legend = "bottom"
    )
    ggsave(file.path("figures", paste0(region, "_", model_selected, "_", scenario_baseline, "_", scenario_delayed, "_profitovertime.png")), width = 6, height = 7)
  }
  
  # discount profits to base year
  df <- df %>% filter(year >= base_year) %>%
    mutate(time_from_baseyear = year - base_year,
           discount_factor = (1 + discount_rate)^(-time_from_baseyear),
           profit_pv = discount_factor*profit
    )
  
  
  if(create_charts) {
    df %>%
      ggplot(aes(year, profit_pv/10^9)) + geom_col(aes(fill = scenario), position = position_dodge2()) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
      labs(y = "PV profit in USD bn", colour = NULL) + facet_wrap(~ scenario) +
      coord_cartesian(xlim = c(NA, 2060)) +
      theme_wsj()
    ggsave(file.path("figures", paste0(region, "_", model_selected, "_", scenario_baseline, "_", scenario_delayed, "_presentvalueprofitovertime.png")), width = 6, height = 7)
  }
  
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
# read_csv(file.path("data", "output", "ar6_database_coal", "IND.csv")) %>% calculate_npv_loss()
# 
# scenario_baseline = "EN_INDCi2100"
# scenario_delayed = "EN_INDCi2030_800f"
# carbonintensity_g_per_kwh = 880
# kwh_per_exajoule = 277777777777.7778
# discount_rate = 0.05
# base_year = 2020
# create_charts = T

iso3_codes_available <- file.path("data", "output", "ar6_database_coal") %>% list.files() %>% str_remove("\\.csv$")

df_input <- crossing(model_selected = c("IMAGE 3.0", "POLES ENGAGE", "REMIND-MAgPIE 2.1-4.2"),
                     region = iso3_codes_available)

wrapper_function <- function(model_selected = NULL,
                             region = NULL) {
  read_csv(file.path("data", "output", "ar6_database_coal", paste0(region, ".csv"))) %>%
    calculate_npv_loss(model = model_selected, create_charts = F)
}

# for(jj in 1:nrow(df_input)) {
#   paste(df_input$model_selected[jj], df_input$region[jj]) %>% print()
#   wrapper_function(df_input$model_selected[jj], df_input$region[jj])
# }

df <- pmap_dfr(df_input, wrapper_function)

df %>% ggplot(aes(region, model)) + geom_tile(aes(fill = npv_change_relative)) +
  geom_label(aes(label = round(npv_change_relative, 2)))

df %>% ggplot(aes(region, model)) + geom_tile(aes(fill = npv_change/10^12)) +
  geom_label(aes(label = round(npv_change/10^12, 2)))