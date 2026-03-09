# 1. Load the library (this contains the data)
library(dplyr)
library(ggplot2)

# Make the dataset available in your environment
# The dataset is already available after loading ggplot2,
# or you can explicitly load it without assigning the return value.
data("diamonds", package = "ggplot2")

# Create a new dataframe called diamonds_clean that:
# Includes only diamonds with a cut of "Ideal" or "Premium".
# Excludes the columns x, y, and z.
# Adds a new column called price_per_carat (Price divided by Carat).

diamonds_clean <- diamonds %>% filter(cut %in% c("Ideal", "Premium")) %>% select(-(x:z)) %>% mutate(price_per_carat = price / carat)

print(diamonds_clean)

# 3. Using diamonds_clean, find the average price and average price_per_carat for "Ideal" vs "Premium" cuts.
summary_tbl <- diamonds_clean %>% group_by(cut) %>% summarise(
    average_price = mean(price),
    average_price_per_carat = mean(price_per_carat))

print(summary_tbl)

# 4. Create a histogram of price with 50 bins to see the distribution.
histogram <- ggplot(diamonds, aes(price)) + geom_histogram(bins = 50, na.rm = TRUE)
print(histogram)