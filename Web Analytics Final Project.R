# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
stock_data <- read.csv("/Users/karimegonzalez/Dropbox/Mac/Desktop/WEB ANALYTICS/Group Project/archive (3)/Salesforce_stock_history.csv")

# Convert Date column to Date type
stock_data$Date <- as.Date(stock_data$Date)

# Overview of the data
str(stock_data)
summary(stock_data)

# Descriptive Statistics: Calculate mean, standard deviation, and percentiles for Open and Close prices
descriptive_stats <- stock_data %>%
  summarise(
    Mean_Open = mean(Open, na.rm = TRUE),
    SD_Open = sd(Open, na.rm = TRUE),
    P25_Open = quantile(Open, 0.25, na.rm = TRUE),
    P50_Open = median(Open, na.rm = TRUE),
    P75_Open = quantile(Open, 0.75, na.rm = TRUE),
    Mean_Close = mean(Close, na.rm = TRUE),
    SD_Close = sd(Close, na.rm = TRUE),
    P25_Close = quantile(Close, 0.25, na.rm = TRUE),
    P50_Close = median(Close, na.rm = TRUE),
    P75_Close = quantile(Close, 0.75, na.rm = TRUE)
  )
print(descriptive_stats)

# Add a column for year to analyze trends over time
stock_data$Year <- format(stock_data$Date, "%Y")

# Visualization 1: Stock Price Over Time (Line Plot)
ggplot(stock_data, aes(x = Date)) +
  geom_line(aes(y = Open, color = "Open Price"), size = 1) +
  geom_line(aes(y = Close, color = "Close Price"), size = 1) +
  labs(title = "Salesforce Stock Prices Over Time", x = "Date", y = "Price") +
  theme_minimal() +
  scale_color_manual(values = c("Open Price" = "blue", "Close Price" = "red"))

# Visualization 2: Box Plot of Stock Prices by Year
ggplot(stock_data, aes(x = Year, y = Close, fill = Year)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  labs(title = "Yearly Distribution of Stock Closing Prices",
       x = "Year", y = "Closing Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Visualization 3: Histogram of Stock Prices
ggplot(stock_data, aes(x = Close)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Salesforce Stock Closing Prices",
       x = "Closing Price", y = "Frequency") +
  theme_minimal()

# Additional Insight: Identify Periods of Stability and Growth
# Calculate daily percentage change in closing price
stock_data <- stock_data %>%
  arrange(Date) %>%
  mutate(
    Daily_Percent_Change = (Close - lag(Close)) / lag(Close) * 100
  )

# Filter periods with low variability (stability) and high growth
stability_period <- stock_data %>%
  filter(Daily_Percent_Change > -0.5 & Daily_Percent_Change < 0.5)

growth_period <- stock_data %>%
  filter(Daily_Percent_Change > 1)

print("Periods of Stability:")
print(stability_period)

print("Periods of Growth:")
print(growth_period)

# Visualization 4: Daily Percentage Change Over Time
ggplot(stock_data, aes(x = Date, y = Daily_Percent_Change)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Daily % Change in Salesforce Stock Closing Prices",
       x = "Date", y = "Daily % Change") +
  theme_minimal()

