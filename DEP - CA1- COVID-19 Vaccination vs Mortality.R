# Set working directory
setwd("C:/Users/danrl/OneDrive/Documents/R Programming/DEP-CA1-Project/")

# Load dataset
covid_dataset = read.csv("covid-vaccination-vs-death_ratio.csv")

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)

# 1. Data Overview

# Display first and last rows
head(covid_dataset)
tail(covid_dataset)

# Get dimensions
dim(covid_dataset)

# Display stats summary of dataset
summary(covid_dataset)

# 2. Data Cleaning

# Renaming column
colnames(covid_dataset)[8] <- "new_deaths"

# Drop unnecessary column
covid_dataset <- subset(covid_dataset, select = -X)
