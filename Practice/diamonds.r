# ---- Load Packages ----
library(dplyr)
library(ggplot2)

# ---- Data ----
# The diamonds dataset is included with ggplot2.
data("diamonds", package = "ggplot2")

# Quick inspection (RStudio-friendly)
glimpse(diamonds)
View(diamonds)

# ---- Data Wrangling ----
# Create a new dataframe called diamonds_clean that:
# - Includes only diamonds with a cut of "Ideal" or "Premium"
# - Excludes the columns x, y, and z
# - Adds a new column called price_per_carat (Price divided by Carat)
diamonds_clean <- diamonds %>%
  filter(cut %in% c("Ideal", "Premium")) %>%
  select(-(x:z)) %>%
  mutate(price_per_carat = price / carat)

glimpse(diamonds_clean)
View(diamonds_clean)

# ---- Summary Table ----
# Average price and average price_per_carat for Ideal vs Premium cuts
summary_tbl <- diamonds_clean %>%
  group_by(cut) %>%
  summarise(
    average_price = mean(price),
    average_price_per_carat = mean(price_per_carat),
    .groups = "drop"
  )

print(summary_tbl)
View(summary_tbl)

# ---- Visualizations ----
# Histogram of price with 50 bins
price_histogram <- ggplot(diamonds, aes(price)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  theme_minimal()

price_histogram

# Boxplot of price_per_carat by color
price_per_carat_boxplot <- ggplot(diamonds_clean, aes(x = color, y = price_per_carat)) +
  geom_boxplot() +
  theme_minimal()

price_per_carat_boxplot