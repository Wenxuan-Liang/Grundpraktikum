##### Question 1 ######
##### Electricity and national income #####
# How does access to electricity correlate with adjusted net national income
# per capita across different countries? Does this also depend on the country
# size or the inhabitants?
#########################
Worldbank <- readr::read_rds("Data/Worldbank.RDS")

summary(Worldbank)
str(Worldbank)

Worldbank_Q1 <- Worldbank %>%
  filter(Year < 2023) %>%
  select("Country Name":"Adjusted net national income per capita (current US$)", "Surface area (sq. km)", "Population, total")
    

Worldbank_Q1 %>%
  filter(Year < 2022) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  scale_y_log10()+
  facet_grid(rows = vars(Year))+
  theme_light()

Worldbank_Q1 %>%
  filter(Year < 2022) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  geom_line()+
  scale_y_log10()+
  facet_grid(cols = vars(`Country Code`))+
  theme_light()

Worldbank_Q1 %>%
  filter(Year == 2021) %>%
  ggplot(aes(x = `Access to electricity (% of population)`, y = `Adjusted net national income per capita (current US$)`)) +
  geom_point(color = "blue", alpha = .5, shape = "diamond", size = 2)+
  scale_y_log10()+
  theme_light()

Worldbank_Q1 %>%
  mutate(Electricity_75 = `Access to electricity (% of population)`>= 75) %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = Electricity_75))+
  geom_boxplot() +
  scale_y_log10()

Worldbank_Q1 %>%
  filter(is.na(`Access to electricity (% of population)`))

Plot_Electricity1 <- Worldbank_Q1 %>%
  ggplot(aes(y = `Adjusted net national income per capita (current US$)`, x = cut_interval(`Access to electricity (% of population)`, n = 4, breaks = c(0,25,50,75,100), labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"))))+
  geom_point(aes(colour = `Country Code` == 'AFG')) +
  geom_boxplot()+
  scale_y_log10() +
  labs(color = "Country")+
  scale_color_manual(labels = c("Rest of World", "Afghanistan"), values = c("darkgrey", "red"))+
  ylab("Adj. net national income p.c. (current US$)")+
  xlab("Access to electricity")+
  theme_light()

ggsave("Plots/Q1-1.png", Plot_Electricity1, device = "png")


Worldbank_Q1 %>%
  filter(`Adjusted net national income per capita (current US$)` < 1000) %>%
  filter(`Access to electricity (% of population)` > 76.9)

cut(Worldbank_Q1$`Access to electricity (% of population)`, breaks = c(0,25,50,75,100))

cut_interval(Worldbank_Q1$`Access to electricity (% of population)`,n = 4, breaks = c(0,25,50,75,100), labels = c("(0,25]", "(25,50]", "(50,75]", "(75,100]"))
