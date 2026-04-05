library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read CSV
budget <- read_csv("Practice/budget_spreadsheet.csv", col_names = TRUE)

# Name columns
colnames(budget)[1:3] <- c("date", "description", "amount")

# Mutate
budget <- budget %>% mutate(date = dmy(date))

# Spent
spend <- budget %>% filter(amount < 0)

# Daily spend totals
daily_spend <- spend %>% group_by(date) %>% summarise(total_spent = -sum(amount), .groups = "drop")

# Bar chart: spending per day
daily_spend_plot <- ggplot(daily_spend, aes(x = date, y = total_spent)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Daily Spending", x = "Date", y = "Total Spent")

daily_spend_plot

# Average spending by weekday
weekday_avg <- daily_spend %>%
  mutate(weekday = wday(date, label = TRUE, week_start = 1)) %>%
  group_by(weekday) %>%
  summarise(avg_spent = mean(total_spent), .groups = "drop")

weekday_avg

# Optional: weekday averages bar chart
weekday_avg_plot <- ggplot(weekday_avg, aes(x = weekday, y = avg_spent)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Spending by Weekday", x = "Weekday", y = "Average Spent")

weekday_avg_plot

