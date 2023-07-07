require(tidyverse)

df_expense <- read.csv("data/input/WorldBank_PublicExpense_data.csv")

df_expense %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year") %>% 
  mutate(Year= str_sub(string=Year,start = -5L,end = -2L) %>% as.numeric(),
         value=as.numeric(value)) %>% 
  na.omit() %>% 
  select(-Series.Code) %>% 
  pivot_wider(names_from = Series.Name,values_from = value) %>% 
  filter(Country.Code == "IND") %>%
  mutate(PublicExpense_tn2015USD = `Expense (% of GDP)`/100 * `GDP (constant 2015 US$)` /1e9,
    InterestExpense_2015USD = `Interest payments (% of expense)`/100 * PublicExpense_tn2015USD) %>% 
  na.omit() %>% 
  summarise(avg_exp = mean(`Expense (% of GDP)`), avg_intexp = mean(`Interest payments (% of expense)`)) -> .a

avg_exp <- .a$avg_exp
avg_intexp <- .a$avg_intexp

df_loss <- read.csv("data/output/annual_profits_coal/IND_REMIND-MAgPIE 2.1-4.2_EN_INDCi2100_EN_INDCi2030_800f.csv")

df_loss <-  df_loss %>% 
  select(Scenario=scenario,Year=year,profit) %>% 
   pivot_wider(names_from = Scenario,values_from = profit) %>% 
  mutate(annual_loss=EN_INDCi2030_800f-EN_INDCi2100)

df <- read.csv("C:/Users/calcm/Documents/GitHub/PowerStrandingAR6/data/input/AR6_Scenarios_Database_ISO3_v1.1.csv")

df <- df %>% 
  filter(Model == "REMIND-MAgPIE 2.1-4.2",
         Scenario %in% c("EN_INDCi2100","EN_INDCi2030_800f"),
         Variable %in% c("GDP|MER","Price|Carbon","Secondary Energy|Electricity|Coal"),
         Region == "IND") %>% 
  pivot_longer(cols = starts_with("X"),names_to = "Year") %>% 
  mutate(Year = str_remove(Year, "X") %>% as.integer())

df_compare <- df %>% 
  select(-Unit) %>% 
  pivot_wider(names_from = Variable,values_from = value) %>% 
  rename(GDP = `GDP|MER`, carbonprice = `Price|Carbon`,secondary_energy_electricity_coal = `Secondary Energy|Electricity|Coal`) %>% 
  filter(Year > 2019, Year < 2101) %>%
  group_by(Scenario) %>% 
  arrange(Scenario,Year) %>% 
  complete(Year = seq(2020,2100)) %>%
  fill(Model,Region) %>% 
  mutate(GDP_interpolated = zoo::na.approx(GDP, method = "linear", rule = 2),
         carbonprice_interpolated = zoo::na.approx(carbonprice, method = "linear", rule = 2),
         electricity_interpolated = zoo::na.approx(secondary_energy_electricity_coal, method = "linear", rule = 2),
         avg_exp = avg_exp/100,
         avg_intexp = avg_intexp/100,
         PublicExpense = avg_exp * GDP_interpolated,
         InterestExpense=avg_intexp * PublicExpense,
         carbon_revenues = 10^(-6)*880*(electricity_interpolated*277777777777.7778)*carbonprice_interpolated/1e9)

ggplot() +
  geom_area(data = df_compare %>% 
              select(Scenario,Year,carbon_revenues) %>% 
              pivot_wider(names_from = Scenario,values_from = carbon_revenues) %>% 
              mutate(annual_gain=EN_INDCi2030_800f-EN_INDCi2100) %>% 
              left_join(df_loss %>% select(Year,annual_loss)) %>% 
              mutate(annual_loss=annual_loss*1e-9) %>% 
              pivot_longer(cols = starts_with("annual"), names_to = "variable"),
            aes(x=Year,y=value,fill=variable),
            alpha=0.2) +
  geom_area(data = df_compare %>% 
              select(Scenario,Year,carbon_revenues) %>% 
              pivot_wider(names_from = Scenario,values_from = carbon_revenues) %>% 
              mutate(annual_gain=EN_INDCi2030_800f-EN_INDCi2100) %>% 
              left_join(df_loss %>% select(Year,annual_loss)) %>% 
              mutate(annual_loss=annual_loss*1e-9 *0.604) %>% 
              pivot_longer(cols = starts_with("annual"), names_to = "variable"),
    aes(x=Year,y=value,fill=variable),
    alpha=0.7) +
  geom_line(data = df_compare %>% 
              select(Scenario,Year,carbon_revenues) %>% 
              pivot_wider(names_from = Scenario,values_from = carbon_revenues) %>% 
              mutate(annual_gain=EN_INDCi2030_800f-EN_INDCi2100) %>% 
              left_join(df_loss %>% select(Year,annual_loss)) %>% 
              mutate(annual_loss=annual_loss*1e-9,#*0.604,
                     delta = annual_gain + annual_loss),
            aes(x=Year,y=delta),
            linetype=2) +
  geom_line(data = df_compare %>% 
              select(Scenario,Year,carbon_revenues) %>% 
              pivot_wider(names_from = Scenario,values_from = carbon_revenues) %>% 
              mutate(annual_gain=EN_INDCi2030_800f-EN_INDCi2100) %>% 
              left_join(df_loss %>% select(Year,annual_loss)) %>% 
              mutate(annual_loss=annual_loss*1e-9*0.604,
                     delta = annual_gain + annual_loss),
            aes(x=Year,y=delta)) +
  coord_cartesian(xlim = c(NA,2060)) +
  theme_classic() +
  theme(strip.background = element_blank(), 
        #strip.text = element_text(hjust = 0, face = "bold", colour = "#595959"),
        strip.text = element_blank(),
        text=element_text(size=12, family = "sans"),
        axis.text = element_text(colour = "#595959"),
        axis.line = element_line(colour = "#B3B3B3"),
        axis.ticks = element_line(colour = "#B3B3B3"),
        plot.subtitle = element_text(colour = "#595959"),
        plot.title = element_text(face = "bold", colour = "#1A1A1A"),
        legend.position = "none") +
  scale_fill_manual(values = c("#141F52", "#E3120B")) +
  labs(y= "2010 USD bn",
       x=element_blank(),
       title = "The net cash flow effect is uncertain",
       subtitle = "Carbon revenues from coal electricity VS public and total loss from stranding")

ggsave(plot=last_plot(),filename = "figures/netcashflow.png",width = 14,height  = 7.5,dpi = 800)


  


df_compare %>% 
  select(Scenario,Year,PublicExpense,InterestExpense,annual_loss) %>% 
  pivot_longer(cols = c("PublicExpense","InterestExpense","annual_loss"), names_to = "Variable" ) %>% 
  filter(Year <= 2050) %>% 
  ggplot(aes(x=Year,y=value,color=Variable, linetype = Scenario)) +geom_line()


