# Install and load the required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

library(ggplot2)
library(dplyr)
library(readr)

# Read the dataset
data <- read_csv("G3_sydney_hobart_times.csv")

# Display the first few rows
head(data)

# Display the last few rows
tail(data)

# Check the dimensions of the dataset
dim(data)

# Check for missing values
sum(is.na(data))

# Perform descriptive statistics
summary(data)

# Clean the 'Time' column by extracting only the numeric values
data$Time <- as.numeric(gsub("[^0-9.]+", "", data$Time))

# Check the 'Time' column after cleaning
head(data$Time)
sum(is.na(data$Time))

# Calculate the mean time
mean_time <- mean(data$Time, na.rm = TRUE)

# Fill missing values in the 'Time' column with the mean
data$Time[is.na(data$Time)] <- mean_time

# Check for missing values in the 'Time' column again
sum(is.na(data$Time))

# Round the 'Time' column to two decimal places
data$Time <- round(data$Time, 2)

# Perform calculations on 'Code Time less than 3'
data$`Time less than 3` <- ifelse(data$Time < 3, "Less than 3", "More than 3")

# Calculate the difference between 'fleet_start' and 'fleet_finish' to get the completion time
data$`Completed the race` <- data$fleet_start - data$fleet_finish

# Find the year and value with the least 'Completed the race'
min_row <- data[which.min(data$`Completed the race`), ]
min_year <- min_row$Year
min_value <- min_row$`Completed the race`

# Find the year and value with the highest 'Completed the race'
max_row <- data[which.max(data$`Completed the race`), ]
max_year <- max_row$Year
max_value <- max_row$`Completed the race`

# Scatter plot of 'Year' vs 'Completed the race'
scatter_plot <- ggplot(data, aes(x = Year, y = `Completed the race`)) +
  geom_point(color = "blue") +
  geom_point(data = min_row, aes(x = min_year, y = min_value), color = "red", show.legend = FALSE) +
  geom_point(data = max_row, aes(x = max_year, y = max_value), color = "green", show.legend = FALSE) +
  geom_text(data = min_row, aes(x = min_year, y = min_value, label = min_year), nudge_y = 5, show.legend = FALSE) +
  geom_text(data = max_row, aes(x = max_year, y = max_value, label = max_year), nudge_y = 5, show.legend = FALSE) +
  labs(x = "Year", y = "Completed the race", title = "Difference in Start and End") +
  theme_bw()

# Group the data by 'Year' and calculate the total fleet starts for each year
fleet_starts <- data %>%
  group_by(Year) %>%
  summarize(total_fleet_starts = sum(fleet_start)) %>%
  arrange(desc(total_fleet_starts))

# Bar plot to visualize the years with the most fleet starts
bar_plot <- ggplot(fleet_starts, aes(x = Year, y = total_fleet_starts)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Year", y = "Fleet Starts") +
  ggtitle("Years with the Most Fleet Starts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count the occurrences of each 'Time' value
time_counts <- count(data, Time)

# Bar chart to visualize the number of fleet finishes by time
time_bar_chart <- ggplot(time_counts, aes(x = Time, y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Time", y = "Number of Fleet Finishes") +
  ggtitle("Number of Fleet Finishes by Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count the occurrences of 'Time less than 3' and 'Time more than 3'
time_less_than_3_counts <- count(data, `Time less than 3`)

# Pie chart to visualize the counts of 'Time less than 3' and 'Time more than 3'
pie_chart <- ggplot(time_less_than_3_counts, aes(x = "", y = n, fill = `Time less than 3`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(fill = "Time") +
  ggtitle("Time less than 3") +
  theme_void()

# Print the resulting plots
lapply(list(pie_chart, scatter_plot, bar_plot, time_bar_chart), print)

