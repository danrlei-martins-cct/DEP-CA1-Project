# Set working directory
setwd("C:/Users/danrl/OneDrive/Documents/R Programming/DEP-CA1-Project/")

# Load dataset
covid_dataset = read.csv("covid-vaccination-vs-death_ratio.csv")

# Install and load necessary libraries
install.packages(c("ggplot2", "tidyr", "dplyr", "viridis", "scales", "corrplot", "reshape2", "maps", "mapdata"))
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(scales)
library(corrplot)
library(reshape2)
library(maps)
library(mapdata)

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

# Dealing with negative values (new_deaths)

# Check minimum value of new_deaths
summary(covid_dataset$new_deaths)

negative_deaths <- covid_dataset[covid_dataset$new_deaths < 0, ]

# Assuming the negative values are typos, fixing the values
covid_dataset$new_deaths <- ifelse(covid_dataset$new_deaths < 0, -covid_dataset$new_deaths, covid_dataset$new_deaths)

# 3. Variable Identification and Visualization

# Display structure of dataset, showing the data types
cat("\nStructure of the dataset:\n")
str(covid_dataset)

# 3.1 Categorical variable - country

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

# 3.2 Discrete variable - date
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

# 3.3 Continuous Variables - people_vaccinated & people_fully_vaccinated
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

# 3.4 Continuous variable - new_deaths
ggplot(covid_dataset, aes(x = date, y = new_deaths)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "New Deaths Over Time",
       x = "Date",
       y = "Number of New Deaths") +
  theme_minimal()

# 3.5 Continuous variable - ratio
# to be finished

# 4. Missing Values Exploration

# Check for missing data
cat("\nMissing Data:\n")
print(colSums(is.na(covid_dataset)))

# 5. Statistical Parameters Calculation

mean_total_vaccinations <- mean(covid_dataset$total_vaccinations)
median_total_vaccinations <- median(covid_dataset$total_vaccinations)
min_total_vaccinations <- min(covid_dataset$total_vaccinations)
max_total_vaccinations <- max(covid_dataset$total_vaccinations)
sd_total_vaccinations <- sd(covid_dataset$total_vaccinations)

mean_people_vaccinated <- mean(covid_dataset$people_vaccinated)
median_people_vaccinated <- median(covid_dataset$people_vaccinated)
min_people_vaccinated <- min(covid_dataset$people_vaccinated)
max_people_vaccinated <- max(covid_dataset$people_vaccinated)
sd_people_vaccinated <- sd(covid_dataset$people_vaccinated)

mean_people_fully_vaccinated <- mean(covid_dataset$people_fully_vaccinated)
median_people_fully_vaccinated <- median(covid_dataset$people_fully_vaccinated)
min_people_fully_vaccinated <- min(covid_dataset$people_fully_vaccinated)
max_people_fully_vaccinated <- max(covid_dataset$people_fully_vaccinated)
sd_people_fully_vaccinated <- sd(covid_dataset$people_fully_vaccinated)

mean_new_deaths <- mean(covid_dataset$new_deaths)
median_new_deaths <- median(covid_dataset$new_deaths)
min_new_deaths <- min(covid_dataset$new_deaths)
max_new_deaths <- max(covid_dataset$new_deaths)
sd_new_deaths <- sd(covid_dataset$new_deaths)

mean_population <- mean(covid_dataset$population)
median_population <- median(covid_dataset$population)
min_population <- min(covid_dataset$population)
max_population <- max(covid_dataset$population)
sd_population <- sd(covid_dataset$population)

mean_ratio <- mean(covid_dataset$ratio)
median_ratio <- median(covid_dataset$ratio)
min_ratio <- min(covid_dataset$ratio)
max_ratio <- max(covid_dataset$ratio)
sd_ratio <- sd(covid_dataset$ratio)

# 6. Normalization and Standardization

# Extract numerical variables
numerical_vars <- covid_dataset %>%
  select(total_vaccinations, people_vaccinated, people_fully_vaccinated, new_deaths, population, ratio)

## Min-Max Normalization
covid_min_max_normalized <- as.data.frame(apply(numerical_vars, 2, function(x) (x - min(x)) / (max(x) - min(x))))

## Z-score Standardization
covid_z_score_standardized <- as.data.frame(scale(numerical_vars))

## Robust Scalar

# Define the robust_scalar function
robust_scalar <- function(x) {
  (x - median(x)) / (quantile(x, probs = .75) - quantile(x, probs = .25))
}

# Apply the robust_scalar function to each column
covid_robust_scaled <- numerical_vars %>%
  mutate_all(~robust_scalar(.))

# 7. Correlation Analysis

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_vars)

# Reshape the correlation matrix for plotting
correlation_data <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_data) <- c("Var1", "Var2", "Correlation")

# Line
ggplot(correlation_data, aes(x = Var1, y = Correlation, group = Var2, color = Var2)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Correlation Between Variables",
       x = "Variables",
       y = "Correlation")

# Scatter
ggplot(covid_dataset, aes(x = total_vaccinations, y = new_deaths)) +
  geom_point(color = "steelblue") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Scatter Plot of New Deaths vs Total Vaccinations",
       x = "Total Vaccinations",
       y = "New Deaths") +
  theme_minimal() 

# Heatmap
ggplot(melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Between Variables",
       x = "Variables",
       y = "Variables")

