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
