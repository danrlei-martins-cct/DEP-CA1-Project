# Set working directory
setwd("C:/Users/danrl/OneDrive/Documents/R Programming/DEP-CA1-Project/")

# Load dataset
covid_dataset = read.csv("covid-vaccination-vs-death_ratio.csv")

# Install and load necessary libraries
install.packages(c("ggplot2", "tidyr", "dplyr", "viridis", "scales", "corrplot", "reshape2", "maps", "mapdata", "fastDummies", "stats", "FactoMineR"))

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(scales)
library(corrplot)
library(reshape2)
library(maps)
library(mapdata)
library(fastDummies)
library(stats)
library(FactoMineR)

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

# Renaming column for consistency
colnames(covid_dataset)[8] <- "new_deaths"

# Drop unnecessary columns
covid_dataset <- subset(covid_dataset, select = -X)
covid_dataset <- subset(covid_dataset, select = -ratio)

# Creating percentage of vaccinated and fully vaccinated columns
covid_dataset <- covid_dataset %>%
  mutate(pc_vaccinated = people_vaccinated / population * 100,
         pc_fully_vacc = people_fully_vaccinated / population * 100)

# Convert the date column to Date type
covid_dataset$date <- as.Date(covid_dataset$date)

# Dealing with negative values (new_deaths)

# Check minimum value of new_deaths
summary(covid_dataset$new_deaths)

negative_deaths <- covid_dataset[covid_dataset$new_deaths < 0, ]

# Assuming the negative values are typos, fixing the values
covid_dataset$new_deaths <- ifelse(covid_dataset$new_deaths < 0, -covid_dataset$new_deaths, covid_dataset$new_deaths)

# 3. Variable Identification and Visualization

# Display structure of data set, showing the data types
cat("\nStructure of the dataset:\n")
str(covid_dataset)

# 3.1 Categorical variable - country

# Select the top 10 countries by total vaccinations
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
  theme(axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values = c("steelblue")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, scale = 1e6, suffix = " M")) +
  geom_text(aes(label = format(round(total_vaccinations), big.mark = ",")), 
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
  theme(legend.position = "right") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))

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

mean_pc_vaccinated <- mean(covid_dataset$pc_vaccinated)
median_pc_vaccinated <- median(covid_dataset$pc_vaccinated)
min_pc_vaccinated <- min(covid_dataset$pc_vaccinated)
max_pc_vaccinated <- max(covid_dataset$pc_vaccinated)
sd_pc_vaccinated <- sd(covid_dataset$pc_vaccinated)

mean_pc_fully_vacc <- mean(covid_dataset$pc_fully_vacc)
median_pc_fully_vacc <- median(covid_dataset$pc_fully_vacc)
min_pc_fully_vacc <- min(covid_dataset$pc_fully_vacc)
max_pc_fully_vacc <- max(covid_dataset$pc_fully_vacc)
sd_pc_fully_vacc <- sd(covid_dataset$pc_fully_vacc)

# Create a vector for each parameter and statistic
parameters <- rep(c("Total Vaccinations", "People Vaccinated", "People Fully Vaccinated", "New Deaths", "Population", "Partially Vaccinated %", "Fully Vaccinated %"), each = 5)
statistics <- rep(c("Mean", "Median", "Min", "Max", "SD"), times = 7)

# Create a vector for each calculated value
values <- c(
  mean(covid_dataset$total_vaccinations), median(covid_dataset$total_vaccinations), min(covid_dataset$total_vaccinations), max(covid_dataset$total_vaccinations), sd(covid_dataset$total_vaccinations),
  mean(covid_dataset$people_vaccinated), median(covid_dataset$people_vaccinated), min(covid_dataset$people_vaccinated), max(covid_dataset$people_vaccinated), sd(covid_dataset$people_vaccinated),
  mean(covid_dataset$people_fully_vaccinated), median(covid_dataset$people_fully_vaccinated), min(covid_dataset$people_fully_vaccinated), max(covid_dataset$people_fully_vaccinated), sd(covid_dataset$people_fully_vaccinated),
  mean(covid_dataset$new_deaths), median(covid_dataset$new_deaths), min(covid_dataset$new_deaths), max(covid_dataset$new_deaths), sd(covid_dataset$new_deaths),
  mean(covid_dataset$population), median(covid_dataset$population), min(covid_dataset$population), max(covid_dataset$population), sd(covid_dataset$population),
  mean(covid_dataset$pc_vaccinated), median(covid_dataset$pc_vaccinated), min(covid_dataset$pc_vaccinated), max(covid_dataset$pc_vaccinated), sd(covid_dataset$pc_vaccinated),
  mean(covid_dataset$pc_fully_vacc), median(covid_dataset$pc_fully_vacc), min(covid_dataset$pc_fully_vacc), max(covid_dataset$pc_fully_vacc), sd(covid_dataset$pc_fully_vacc)
)

# Create the summary_stats data frame
summary_stats <- data.frame(Parameter = parameters, Statistic = statistics, Value = values)

# Turn off scientific notation for results
for (i in 1:nrow(summary_stats)) {
  summary_stats$Value[i] <- format(summary_stats$Value[i], scientific = FALSE)
}

print(summary_stats)

# 6. Normalization and Standardization

# Extract numerical variables
numerical_vars <- covid_dataset %>%
  select(total_vaccinations, people_vaccinated, people_fully_vaccinated, new_deaths, population, pc_vaccinated, pc_fully_vacc)

## Min-Max Normalization
covid_min_max_normalized <- as.data.frame(apply(numerical_vars, 2, function(x) (x - min(x)) / (max(x) - min(x))))

# Visualize Min-Max Normalized Data
ggplot(melt(covid_min_max_normalized), aes(x = variable, y = value, color = variable)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Min-Max Normalized Data") + 
  theme(legend.position = "none")

## Z-score Standardization
covid_z_score_standardized <- as.data.frame(scale(numerical_vars))

# Visualize Z-score Standardized Data
ggplot(melt(covid_z_score_standardized), aes(x = variable, y = value)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Z-score Standardized Data") + 
  theme(legend.position = "none")

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

# Heat map
ggplot(melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Between Variables",
       x = "Variables",
       y = "Variables")

# 8. Data Exploratory Analysis (EDA)

# Scatter Plot - Total Vaccinations vs New Deaths
ggplot(covid_dataset, aes(x = total_vaccinations, y = new_deaths)) +
  geom_point(color = "steelblue") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Scatter Plot of New Deaths vs Total Vaccinations",
       x = "Total Vaccinations",
       y = "New Deaths") +
  theme_minimal() 

# Creating Map Plot to visualize the total number of deaths per country

# Group by country and summarize total deaths and vaccinations
covid_dataset_eda <- covid_dataset %>%
  group_by(country) %>%
  summarize(
    total_deaths = sum(new_deaths, na.rm = TRUE),
    total_vaccinations = max(total_vaccinations, na.rm = TRUE)
  ) %>%
  as.data.frame()

# Standardize country names
covid_dataset_eda <- covid_dataset_eda %>%
  mutate(
    country = case_when(
      country == "United States of America" ~ "USA",
      country == "Russian Federation" ~ "Russia",
      country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      country == "Antigua and Barbuda" ~ "Antigua",
      country == "Brunei Darussalam" ~ "Brunei",
      country == "Cabo Verde" ~ "Cape Verde",
      country == "Curaçao" ~ "Curacao",
      country == "Falkland Islands (Malvinas)" ~ "Falkland Islands",
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Lao People's Democratic Republic" ~ "Laos",
      country == "Republic of Korea" ~ "South Korea",
      country == "Republic of Moldova" ~ "Moldova",
      country == "Saint Kitts and Nevis" ~ "Saint Kitts",
      country == "Syrian Arab Republic" ~ "Syria",
      country == "The United Kingdom" ~ "UK",
      country == "Trinidad and Tobago" ~ "Trinidad",
      country == "United Republic of Tanzania" ~ "Tanzania",
      country == "Viet Nam" ~ "Vietnam",
      country == "occupied Palestinian territory, including east Jerusalem" ~ "Palestine",
      TRUE ~ country
    ))

world_map <- map_data("world")

# Merge world map with covid_dataset_eda
covid_world <- merge(world_map, covid_dataset_eda, by.x = "region", by.y = "country", all.x = TRUE)

# Set breaks as ranges for labels
breaks <- c(600000, 500000, 400000, 200000, 100000, 0)

# Plot map
ggplot() +
  geom_polygon(data = covid_world, aes(x = long, y = lat, group = group, fill = total_deaths)) +
  scale_fill_viridis(name = "Total Deaths", breaks = breaks, labels = scales::comma(breaks)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = "COVID-19 Dataset: Total Deaths by Country") +
  coord_fixed(ylim = c(-57, 100))

# 9. Dummy Encoding

# Group-by 'country' variable
covid_dataset_by_country <- covid_dataset %>%
  group_by(country) %>%
  summarise(
    total_vaccinations = sum(total_vaccinations),
    people_vaccinated = sum(people_vaccinated),
    people_fully_vaccinated = sum(people_fully_vaccinated),
    new_deaths = sum(new_deaths)
  )

# Apply dummy encoding to the country variable
covid_dummy_encoding <- dummy_cols(covid_dataset_by_country, select_columns = "country", remove_selected_columns = TRUE)

# 10. Principal Component Analysis (PCA)

# Apply PCA
covid_pca <- PCA(numerical_vars, scale.unit = TRUE, graph = FALSE)

# Profile of the first few components
summary(covid_pca)

# Plotting the first few components
plot(covid_pca, choix = "var", title = "PCA - Variables", col.var = "blue", pch.var = 16)

# Profile of the first principal component
summary(covid_pca$var$cor)