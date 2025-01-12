rm(list = ls())

getwd()


install.packages("tzdb")

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")


install.packages("corrplot")     # For correlation plot
install.packages("ggplot2")      # For data visualization

# Load the packages after installation
library(corrplot)
library(ggplot2)

data <- read.csv("est16us.csv", header = TRUE)


# Drop the first row if it contains unnamed or unwanted data
data <- data[-1, ]

head(data)

# Rename the columns as specified
colnames(data) <- c(
  "State FIPS Code",
  "Postal Code",
  "Name",
  "Poverty Estimate (All Ages)",
  "90% CI Lower Bound (All Ages)",
  "90% CI Upper Bound (All Ages)",
  "Poverty Percent (All Ages)",
  "90% CI Lower Bound (All Ages)",
  "90% CI Upper Bound (All Ages)",
  "Poverty Estimate (Age 0-17)",
  "90% CI Lower Bound (Age 0-17)",
  "90% CI Upper Bound (Age 0-17)",
  "Poverty Percent (Age 0-17)",
  "90% CI Lower Bound (Age 0-17)",
  "90% CI Upper Bound (Age 0-17)",
  "Poverty Estimate (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Poverty Percent (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Median Household Income (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Poverty Estimate (Age 0-4)",
  "90% CI Lower Bound (Age 0-4)",
  "90% CI Upper Bound (Age 0-4)",
  "Poverty Percent (Age 0-4)",
  "90% CI Lower Bound (Age 0-4)",
  "90% CI Upper Bound (Age 0-4)"
)

# View the modified data
head(data)

str(data)


library(dplyr)

# Ensure column names are unique
colnames(data) <- make.unique(colnames(data))

# Convert appropriate columns to numeric while preserving column names
data <- data %>%
  mutate(across(
    .cols = -c(`State FIPS Code`, `Postal Code`, Name), # Exclude non-numeric columns
    .fns = ~ as.numeric(.),                            # Convert to numeric
    .names = "{.col}"                                  # Keep original column names
  ))


# Verify the structure of the updated data
str(data)

# Check for any NA values in numeric columns after conversion
summary(data)

# Check for missing values
colSums(is.na(data))

# Drop rows with missing values (if necessary)
data <- na.omit(data)


# Summary statistics of the dataset
summary(data)

head(data)

str(data)

#Question-1(A)

# Scatterplot with Linear Trendline
scatterplot <- ggplot(data, aes(x = `Median Household Income (Age 5-17 in Families)`, y = `Poverty Percent (All Ages)`)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Scatterplot of Poverty Percent vs Median Household Income",
    x = "Median Household Income (Age 5-17 in Families)",
    y = "Poverty Percent (All Ages)"
  ) +
  theme_minimal()

# Print the scatterplot
print(scatterplot)


#Question-1(B)

##Histogram

# Histogram with Normal Curve Overlay
# Calculate mean and standard deviation for the normal curve
mean_poverty <- mean(data$`Poverty Percent (All Ages)`, na.rm = TRUE)
sd_poverty <- sd(data$`Poverty Percent (All Ages)`, na.rm = TRUE)

histogram <- ggplot(data, aes(x = `Poverty Percent (All Ages)`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "skyblue", alpha = 0.7) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean_poverty, sd = sd_poverty),
    color = "red",
    size = 1
  ) +
  labs(
    title = "Histogram of Poverty Percent (All Ages) with Normal Curve",
    x = "Poverty Percent (All Ages)",
    y = "Density"
  ) +
  theme_minimal()

# Print the histogram
print(histogram)




#Question-2(A)

library(ggplot2)

##Boxplot

# Creating the boxplot
ggplot(data, aes(x = cut(`Median Household Income (Age 5-17 in Families)`, breaks = 4), 
                 y = `Poverty Percent (All Ages)`)) +
  geom_boxplot() +
  labs(title = "Poverty Percent by Median Household Income (Age 5-17 in Families)", 
       x = "Median Household Income (Age 5-17 in Families)", 
       y = "Poverty Percent (All Ages))") +
  theme_minimal()



#Question-2(B)

##Histogram

library(ggplot2)

# Creating the histogram with normal curve overlay
histogram <- ggplot(data, aes(x = `Poverty Percent (All Ages)`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "white", alpha = 0.7) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean_poverty, sd = sd_poverty),
    color = "red",
    size = 1
  ) +
  labs(
    title = "Histogram of Poverty Percent (All Ages) with Normal Curve",
    x = "Poverty Percent (All Ages)",
    y = "Density"
  ) +
  theme_minimal()

# Print the histogram
print(histogram)


#Question-3(a)

library(ggplot2)

# Normalizing the data
data_normalized <- data
data_normalized$`Poverty Percent (All Ages)` <- data_normalized$`Poverty Percent (All Ages)` / sum(data_normalized$`Poverty Percent (All Ages)`) * 100

# Creating the normalized stacked bar chart
ggplot(data_normalized, 
       aes(x = cut(`Median Household Income (Age 5-17 in Families)`, breaks = 4), 
           y = `Poverty Percent (All Ages)`, 
           fill = cut(`Median Household Income (Age 5-17 in Families)`, breaks = 4))) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Poverty Percent by Median Household Income", 
       x = "Median Household Income Categories", 
       y = "Normalized Poverty Percent (%)", 
       fill = "Income Categories") +
  theme_minimal()





#Question-3(B)

# Load necessary library
library(ggplot2)
library(ggpubr)

# Check for normality of the variables
# Histogram with a normal curve overlay for Median Household Income
ggplot(data, aes(x = `Median Household Income (Age 5-17 in Families)`)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data$`Median Household Income (Age 5-17 in Families)`, na.rm = TRUE), 
                                         sd = sd(data$`Median Household Income (Age 5-17 in Families)`, na.rm = TRUE)), 
                color = "blue", size = 1) +
  labs(title = "Histogram of Median Household Income with Normal Curve",
       x = "Median Household Income (Age 5-17 in Families)",
       y = "Density")

# Histogram with a normal curve overlay for Poverty Percent
ggplot(data, aes(x = `Poverty Percent (All Ages)`)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data$`Poverty Percent (All Ages)`, na.rm = TRUE), 
                                         sd = sd(data$`Poverty Percent (All Ages)`, na.rm = TRUE)), 
                color = "blue", size = 1) +
  labs(title = "Histogram of Poverty Percent with Normal Curve",
       x = "Poverty Percent (All Ages)",
       y = "Density")

# Perform Shapiro-Wilk test for normality
shapiro_income <- shapiro.test(data$`Median Household Income (Age 5-17 in Families)`)
shapiro_poverty <- shapiro.test(data$`Poverty Percent (All Ages)`)

# Print results
print(shapiro_income)
print(shapiro_poverty)

# Correlation based on normality
if (shapiro_income$p.value > 0.05 & shapiro_poverty$p.value > 0.05) {
  # Both variables are normally distributed, use Pearson's r
  correlation <- cor.test(data$`Median Household Income (Age 5-17 in Families)`, 
                          data$`Poverty Percent (All Ages)`, 
                          method = "pearson")
  print("Using Pearson's r for correlation")
} else {
  # At least one variable is not normally distributed, use Spearman's Rho
  correlation <- cor.test(data$`Median Household Income (Age 5-17 in Families)`, 
                          data$`Poverty Percent (All Ages)`, 
                          method = "spearman")
  print("Using Spearman's Rho for correlation")
}

# Print correlation results
print(correlation)


