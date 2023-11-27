# Set working directory
setwd("C:/Users/danrl/OneDrive/Documents/R Programming/DEP-CA1-Project/")

# Load dataset
covid_dataset = read.csv("covid-vaccination-vs-death_ratio.csv")

# Install and load necessary libraries
install.packages(c("ggplot2", "tidyr", "dplyr", "viridis", "scales"))
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(scales)

# 1. Data Overview

# Display first and last rows
cat("First 6 rows:\n")
print(head(covid_dataset))
cat("\nLast 6 rows:\n")
print(tail(covid_dataset))

# Get dimensions
cat("\nDimensions of the dataset:\n")
print(dim(covid_dataset))

# Display stats summary of dataset
cat("\nSummary of the dataset:\n")
print(summary(covid_dataset))

# 2. Data Cleaning

str(covid_dataset)

# Renaming column
colnames(covid_dataset)[8] <- "new_deaths"

# Drop unnecessary column
covid_dataset <- subset(covid_dataset, select = -X)

# Creating percentage of vaccinated and fully vaccinated columns
covid_dataset <- covid_dataset %>%
  mutate(percentage_vaccinated = people_vaccinated / population * 100,
         percentage_fully_vaccinated = people_fully_vaccinated / population * 100)

# Convert the date column to Date type
covid_dataset$date <- as.Date(covid_dataset$date)

# 3. Variable Identification and Visualization

# Display structure of dataset, showing the data types
cat("\nStructure of the dataset:\n")
str(covid_dataset)

# Categorical variable - country

# Select the top 10 countries by total vaccinations (dataset contains 197)
top_10_countries <- covid_dataset %>%
  group_by(country) %>%
  summarise(total_vaccinations = sum(total_vaccinations, na.rm = TRUE)) %>%
  top_n(10, total_vaccinations)

# Ensure "total_vaccinations" is numeric
top_10_countries$total_vaccinations <- as.numeric(top_10_countries$total_vaccinations)

# Modify the total_vaccinations column
top_10_countries <- top_10_countries %>%
  mutate(total_vaccinations = total_vaccinations / 1e6)

# Create a bar chart with the top 10 countries
ggplot(top_10_countries, 
       aes(x = total_vaccinations, 
           y = reorder(country, total_vaccinations))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Bar Chart for Top 10 Total Vaccinations by Country",
       x = "Total Vaccinations (in millions)", y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 6)) +
  scale_fill_manual(values = c("steelblue")) +
  geom_text(aes(label = scales::comma(total_vaccinations)), 
            hjust = 0.2, 
            size = 3.1, 
            color = "white",
            position = position_stack(vjust = 0.5))

# Discrete variable - date
covid_dataset %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = viridis(1)) +
  scale_x_date(date_labels = "%b %d, %Y") +
  labs(x = "Date", y = "Count", title = "Discrete Variable (date) Distribution")

# Continuous variable - total vaccination
ggplot(subset(covid_dataset, country %in% c("Germany", "The United Kingdom")), 
       aes(x = date, y = total_vaccinations, color = country)) +
  geom_line() +
  labs(title = "Germany vs UK - Total Vaccinations Over Time", x = "Date", y = "Total Vaccinations") +
  theme(legend.position = "right")

# Continuous Variables - people_vaccinated & people_fully_vaccinated
ggplot(subset(covid_dataset, country %in% c("The United Kingdom")), aes(x = date)) +
  geom_area(aes(y = percentage_fully_vaccinated, 
                fill = "Fully Vaccinated"), 
            alpha = 0.7)+
  geom_area(aes(y = percentage_vaccinated, 
                fill = "Partially Vaccinated"), 
            alpha = 0.7) +
  geom_line(aes(y = percentage_fully_vaccinated, 
                color = "Fully Vaccinated"), 
            size = 1) +
  geom_line(aes(y = percentage_vaccinated, 
                color = "Partially Vaccinated"), 
            size = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "UK - Percentage of Population Vaccinated Over Time",
       x = "Date", 
       y = "Percentage of Population") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Fully Vaccinated" = "blue", "Partially Vaccinated" = "orange")) +
  scale_color_manual(values = c("Fully Vaccinated" = "blue", "Partially Vaccinated" = "orange"))

# 4. Missing Values Exploration

# Check for missing data
cat("\nMissing Data:\n")
print(colSums(is.na(covid_dataset)))

