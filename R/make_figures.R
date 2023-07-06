rm(list = ls())

library(tidyverse)
library(extrafont)
font_import()

df_india <- read_csv(file.path("data", "output", "annual_profits_coal",
                               paste0("IND", "_",
                            "REMIND-MAgPIE 2.1-4.2", "_",
                            "EN_INDCi2100_EN_INDCi2030_800f.csv")))

# load the unit dictionary and clean up the variable names using janitor::clean_names()
unit_dictionary <- readRDS(file.path("data", "unit_dictionary.rds")) %>% pivot_wider(names_from = "variable", values_from = "unit") %>% 
  clean_names() %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "unit") %>%
  mutate(clean_label = case_when(variable == "capacity_electricity_coal" ~ "Installed coal-fired capacity",
                                 variable == "om_cost_fixed_electricity_coal_w_o_ccs" ~ "O&M cost",
                                 variable == "price_carbon" ~ "Carbon price",
                                 variable == "price_primary_energy_coal" ~ "Coal price",
                                 variable == "price_secondary_energy_electricity" ~ "Electricity price",
                                 variable == "primary_energy_coal_electricity" ~ "Coal use for generation",
                                 variable == "secondary_energy_electricity_coal" ~ "Coal-fired electricity generation",
                                 TRUE ~ NA_character_
                                 ))

# plot how fundamentals used for calculation evolve over time
df_chart1 <- df_india %>% pivot_longer(cols = c(price_carbon, price_secondary_energy_electricity, price_primary_energy_coal,
                                   capacity_electricity_coal, secondary_energy_electricity_coal, om_cost_fixed_electricity_coal_w_o_ccs),
                          names_to = "variable") %>%
  left_join(unit_dictionary, by = "variable") %>%
  mutate(label = factor(paste0(clean_label, "\n(", unit, ")")))

df_chart1 %>%
  ggplot(aes(year)) +
  geom_vline(xintercept = 2030, linetype = "dashed", colour = "#D9D9D9") +
  geom_ribbon(data = df_chart1 %>% select(year, policy_category_coarse, label, value) %>% distinct() %>%
                pivot_wider(names_from = "policy_category_coarse", values_from = "value"),
              aes(year, ymin = `Delayed action`, ymax = Baseline), fill = "#FCE9ED", alpha = 0.5) +
  geom_line(aes(y = value, colour = policy_category_coarse, linetype = policy_category_coarse)) +
  geom_text(data = tibble(label = factor("Carbon price\n(US$2010/t CO2)", levels = levels(df_chart1$label)),
                          year = 2080, value = 775, 
                          policy_category_coarse = "Delayed action"), aes(y = value, label = policy_category_coarse, colour = policy_category_coarse),
            fontface = "bold", hjust = 1, size = 3) +
  geom_text(data = tibble(label = factor("Carbon price\n(US$2010/t CO2)", levels = levels(df_chart1$label)),
                          year = 2095, value = 150,
                          policy_category_coarse = "Baseline"), aes(y = value, label = policy_category_coarse, colour = policy_category_coarse),
            fontface = "bold", hjust = 1, size = 3) +
  facet_wrap(~ label, scales = "free_y") +
  labs(colour = NULL, y = NULL, x = NULL, linetype = NULL,
       title = "Delayed action drastically alters coal-fired fundamentals",
       subtitle = paste0("Values displayed for ", unique(df_india$region), " using ", unique(df_india$model))) +
  scale_colour_manual(values = c("#141F52", "#E3120B")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  theme_classic() +
  theme(strip.background = element_blank(), legend.position = "none",
        strip.text = element_text(hjust = 0, face = "bold", colour = "#595959"),
        text=element_text(size=12, family = "sans"),
        axis.text = element_text(colour = "#595959"),
        axis.line = element_line(colour = "#B3B3B3"),
        axis.ticks = element_line(colour = "#B3B3B3"),
        plot.subtitle = element_text(colour = "#595959"),
        plot.title = element_text(face = "bold", colour = "#1A1A1A"))
ggsave(file.path("figures", paste0("IND", "_",
                                   "REMIND-MAgPIE 2.1-4.2", "_",
                                   "EN_INDCi2100_EN_INDCi2030_800f_fundamentals.pdf")),
       width = 8, height = 5)

df_chart2 <- df_india %>%
  mutate(cost_total = cost_fuel + cost_om + cost_carbonprice) %>%
  pivot_longer(cols = c(revenue, cost_total, cost_fuel, cost_om, cost_carbonprice, profit)) %>%
  mutate(value_per_exajoule = value / secondary_energy_electricity_coal,
         scenario_label = case_when(scenario == "EN_INDCi2100" ~ "Baseline",
                                    scenario == "EN_INDCi2030_800f" ~ "Delayed action",
                                    TRUE ~ NA_character_))

p1 <- df_chart2 %>% filter(name %in% c("cost_fuel", "cost_om", "cost_carbonprice")) %>%
  mutate(name_clean = case_when(name == "cost_fuel" ~ "Fuel cost",
                                name == "cost_om" ~ "O&M cost",
                                name == "cost_carbonprice" ~ "Carbon price")) %>%
  ggplot(aes(year, value_per_exajoule/10^9)) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_area(aes(fill = name_clean)) +
  geom_line(data = df_chart2 %>% filter(name %in% c("revenue", "profit")), aes(colour = name)) +
  geom_vline(data = tibble(scenario_label = factor(c("Baseline", "Delayed action")),
                           value = c(NA, 2032.2)),
             aes(xintercept = value), linetype = "dashed", colour = "#D9D9D9") +
  geom_point(data = df_chart2 %>% filter(name %in% c("revenue", "profit"), year == 2032,
                                         scenario_label == "Delayed action"), aes(colour = name)) +
  geom_text(data = tibble(scenario_label = factor("Baseline", levels = c("Baseline", "Delayed action")),
                          year = 2035, value_per_exajoule = 22*10^9,
                          name = "revenue",
                          label = "Electricity price"), aes(label = label, colour = name),
            fontface = "bold", hjust = 0, size = 3) +
  geom_text(data = tibble(scenario_label = factor("Baseline", levels = c("Baseline", "Delayed action")),
                          year = 2021, value_per_exajoule = 14*10^9,
                          name = "profit",
                          label = "Margin"), aes(label = label, colour = name),
            fontface = "bold", hjust = 0, size = 3) +
  labs(y = "2010 USD bn/EJ", colour = NULL, x = NULL, fill = "Cost component",
       title = "CO2 price for transition causes negative margins for Indian coal-fired plants",
       subtitle = paste0("Values displayed for ", unique(df_india$model))) + facet_wrap(~ scenario_label, scales = "free_y") +
  coord_cartesian(ylim = c(-30, 50), xlim = c(NA, 2065)) +
  scale_fill_manual(values = c("#EBEDFA", "#D6DBF5", "#7e8ede")) +
  scale_colour_manual(values = c("#1DC9A4", "#F97A1F")) +
  guides(colour = "none") +
  theme_classic() +
  theme(strip.background = element_blank(), legend.position = c(0.4, 0.85),
        strip.text = element_text(hjust = 0, face = "bold", colour = "#595959"),
        text=element_text(size=12, family = "sans"),
        axis.text = element_text(colour = "#595959"),
        axis.line = element_line(colour = "#B3B3B3"),
        axis.ticks = element_line(colour = "#B3B3B3"),
        plot.subtitle = element_text(colour = "#595959"),
        plot.title = element_text(face = "bold", colour = "#1A1A1A"))

p2 <- df_chart1 %>% filter(clean_label == "Coal-fired electricity generation") %>%
  ggplot(aes(year, value)) + geom_col(fill = "#B3B3B3") +
  facet_wrap(~ policy_category_coarse) +
  geom_text(data = tibble(policy_category_coarse = factor("Baseline", levels = c("Baseline", "Delayed action")),
                          year = 2050, value = 4.2,
                          label = "Coal-fired\ngeneration"), aes(label = label), colour = "#B3B3B3",
            fontface = "bold", hjust = 0, size = 3) +
  labs(y = unit_dictionary$unit[unit_dictionary$clean_label == "Coal-fired electricity generation"], x = NULL) +
  coord_cartesian(xlim = c(NA, 2065)) +
  theme_classic() +
  theme(strip.background = element_blank(), 
        #strip.text = element_text(hjust = 0, face = "bold", colour = "#595959"),
        strip.text = element_blank(),
        text=element_text(size=12, family = "sans"),
        axis.text = element_text(colour = "#595959"),
        axis.line = element_line(colour = "#B3B3B3"),
        axis.ticks = element_line(colour = "#B3B3B3"),
        plot.subtitle = element_text(colour = "#595959"),
        plot.title = element_text(face = "bold", colour = "#1A1A1A"))

ggarrange(p1 + theme(axis.text.x = element_blank()), p2, nrow = 2, heights = c(3, 1), align = "v")
ggsave(file.path("figures", paste0("IND", "_",
                                   "REMIND-MAgPIE 2.1-4.2", "_",
                                   "EN_INDCi2100_EN_INDCi2030_800f_margins.pdf")),
       width = 8, height = 5)


df <- read_csv(file.path("data", "output", "df_npv.csv"))

glimpse(df)

df %>% filter(model == "REMIND-MAgPIE 2.1-4.2") %>%

if(create_charts) {
  df %>%
    ggplot(aes(year, profit_pv/10^9)) + geom_col(aes(fill = scenario), position = position_dodge2()) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    labs(y = "PV profit in USD bn", colour = NULL) + facet_wrap(~ scenario) +
    coord_cartesian(xlim = c(NA, 2060)) +
    theme_wsj()
  ggsave(file.path("figures", paste0(region, "_", model_selected, "_", scenario_baseline, "_", scenario_delayed, "_presentvalueprofitovertime.png")), width = 6, height = 7)
}


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
