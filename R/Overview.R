library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


data_full <- Worldbank

head(data_full)
summary(data_full)

##### Plot 1
num_columns <- c("Access to electricity (% of population)",
                 "Adjusted net national income per capita (current US$)",
                 "Agricultural land (% of land area)",
                 "Central government debt, total (% of GDP)",
                 "Children in employment, total (% of children ages 7-14)",
                 "CO2 emissions (metric tons per capita)",
                 "Current health expenditure per capita (current US$)",
                 "Current education expenditure, total (% of total expenditure in public institutions)",
                 "GDP per capita, PPP (constant 2021 international $)",
                 "Government expenditure on education, total (% of GDP)",
                 "Life expectancy at birth, female (years)",
                 "Life expectancy at birth, male (years)",
                 "Prevalence of HIV, total (% of population ages 15-49)",
                 "Prevalence of overweight, weight for height, female (% of children under 5)",
                 "Prevalence of overweight, weight for height, male (% of children under 5)",
                 "Pupil-teacher ratio, primary",
                 "Population, total",
                 "Population density (people per sq. km of land area)",
                 "Surface area (sq. km)")
# Get specific country
data__specific_country <- data_full %>% 
  filter(`Country Name` == "Finland") ##CHANGE COUNTRY HERE

# Reshape data to long format for plotting
data_long <- data__specific_country %>%
  select(Year, all_of(num_columns)) %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value")


ggplot(data_long, aes(x = Year, y = Value)) +
  geom_line() + 
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Overview over Variables over time for a specific country",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Plot 2
# Calculate the average GDP per capita for each country
average_gdp <- data_full %>%
  group_by(`Country Name`) %>%                # Group by country
  summarise(avg_gdp = mean(`GDP per capita, PPP (constant 2021 international $)`, na.rm = TRUE))  # Calculate average GDP, ignoring NA values

# Plot the data using ggplot
ggplot(average_gdp, aes(x = reorder(`Country Name`, avg_gdp), y = avg_gdp)) +
  geom_point(color = "blue", size = 3) +  # Scatter plot with points
  labs(title = "Average GDP per Capita (PPP, constant 2021) by Country",
       x = "Country",
       y = "Average GDP per Capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

