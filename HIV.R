install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggrepel")
library("ggrepel")

Worldbank1_raw <- read_excel("Data/Worldbank1.xlsx")
Worldbank1_raw
Worldbank2_raw <- read_excel("Data/Worldbank2.xlsx")

Worldbank1 <- Worldbank1_raw[1:208,] %>%
  mutate(across(`2023 [YR2023]`:average, as.numeric)) %>%
  select(-average, -`Series Code`) %>%
  pivot_longer(`2023 [YR2023]`:`2014 [YR2014]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")

Worldbank2 <- Worldbank2_raw[1:39,] %>%
  mutate(`2022 [YR2022]` = as.numeric(`2022 [YR2022]`),
         `2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) %>%
  pivot_longer(`2014 [YR2014]`:`2023 [YR2023]`, names_to = "Year", values_to = "data") %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  select(-`Series Code`) %>%
  pivot_wider(names_from = "Series Name", values_from = "data")

Worldbank <- Worldbank1 %>%
  full_join(Worldbank2, by = c("Country Name" = "Country Name", "Country Code" = "Country Code", "Year" = "Year"))


read.csv("Data/Worldbank.csv")
write.csv(Worldbank, "Data/Worldbank.csv")

#How do countries with high child employment rates compare 
#in terms of HIV prevalence in the population aged 15-49?
Worldbank
colnames(Worldbank)
options(tibble.print_max = 1000)
Children_ER <- select(Worldbank, `Children in employment, total (% of children ages 7-14)`)
Children_ER
Children_ER <- Worldbank %>%
  select(`Children in employment, total (% of children ages 7-14)`) %>%
  drop_na()
Children_ER

HIV_R <- Worldbank %>%
  select(`Prevalence of HIV, total (% of population ages 15-49)`) %>%
  drop_na()
HIV_R

print(Worldbank$`Children in employment, total (% of children ages 7-14)`)
CER1 <- filter(Worldbank,`Children in employment, total (% of children ages 7-14)` == 13.92000)
CER1
Country1 <- CER1$`Country Name`
Country1
Year1 <- CER1$Year
Year1
HIV1 <- CER1$`Prevalence of HIV, total (% of population ages 15-49)`
HIV1

CER2 <- filter(Worldbank,`Children in employment, total (% of children ages 7-14)` == 55.89562)
CER2
Country2 <- CER2$`Country Name`  #character(0) #Chad
Country2
Year2 <- CER2$Year
Year2  #numeric（0）#2015
HIV2 <- CER2$`Prevalence of HIV, total (% of population ages 15-49)`
HIV2 #1.4

CER3 <- filter(Worldbank,`Children in employment, total (% of children ages 7-14)` == 34.73484)
CER3
Country3 <- CER3$`Country Name`
Country3
Year3 <- CER3$Year
Year3
HIV3 <- CER3$`Prevalence of HIV, total (% of population ages 15-49)`
HIV3

result1 <- data.frame(
  Country = c("Bolivia", "Chad", "Tanzania"),
  Children_ER = c(13.92000, 55.89562, 34.73484),
  HIVRate = c(0.3, 1.4, 5.4)
)
result1

ggplot(result1, aes(x = Children_ER, y = HIVRate, color = Country)) + #注释上有a
  geom_point() +
  labs(title = "Child employment rates vs HIV prevalence",
       x = "Children in employment (% of children ages 7-14)",
       y = "Prevalence of HIV (% of population ages 15-49)")
  
#Do countries that spend more on healthcare per capita have 
#lower HIV prevalence rates in the 15-49 population?
colnames(Worldbank)
result2 <- Worldbank %>%
  filter(!is.na(`Current health expenditure per capita (current US$)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Current health expenditure per capita (current US$)`, `Prevalence of HIV, total (% of population ages 15-49)`)                                                                                                       
result2

result2.1 <- result2 %>% 
  group_by(`Country Name`) %>% 
  arrange(desc(`Current health expenditure per capita (current US$)`)) %>% 
  filter(row_number() == 1) %>% 
  select(`Country Name`, `Current health expenditure per capita (current US$)`, `Prevalence of HIV, total (% of population ages 15-49)`)
result2.1

ggplot(result2.1, aes(x = `Current health expenditure per capita (current US$)`, 
                      y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                      color = `Country Name`)) +
  geom_point() +
  labs(title = "Countries that spend more on healthcare per capita have lower HIV prevalence rates?",
       x = "Health Expenditure ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10)) 

#Has healthcare per capita spending increased over time for each country, 
#and has it resulted in lower HIV infection rates?
result3 <- Worldbank %>%
  filter(!is.na(`Current health expenditure per capita (current US$)`) & !is.na(`Prevalence of HIV, total (% of population ages 15-49)`)) %>%
  select(`Country Name`, `Year`, `Current health expenditure per capita (current US$)`, `Prevalence of HIV, total (% of population ages 15-49)`)                                                                                                       
result3

result3.1 <- result3 %>% 
  group_by(`Country Name`) %>% 
  arrange(desc(`Current health expenditure per capita (current US$)`)) %>% 
  select(`Country Name`, `Year`, `Current health expenditure per capita (current US$)`, `Prevalence of HIV, total (% of population ages 15-49)`)
result3.1

ggplot(result3.1, aes(x = `Current health expenditure per capita (current US$)`, 
                      y = `Prevalence of HIV, total (% of population ages 15-49)`, 
                      color = `Country Name`)) +
  geom_line() +  
  labs(title = "The relationship between improvements in health for each country and HIV infection rates",
       x = "Health Expenditure ($)",
       y = "Prevalence of HIV (% of population ages 15-49)") +
  theme(plot.title = element_text(size = 10)) 
