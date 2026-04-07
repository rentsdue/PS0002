# Download dataset computers.csv from NTULearn.
library(dplyr)

# Read the computer dataset from CSV.
comp <- read.csv("Week 11/computers.csv", header = TRUE, sep = ",")
# Inspect structure of the dataset.
str(comp)

# Select relevant variables and scale them for clustering.
comp1 <- comp %>%
  select(price, speed, hd, ram, screen, ads, trend) %>%
  mutate(
    price_scal = scale(price),
    speed_scal = scale(speed),
    hd_scal = scale(hd),
    ram_scal = scale(ram),
    screen_scal = scale(screen),
    ads_scal = scale(ads),
    trend_scal = scale(trend)
  ) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

# Verify structure after scaling.
str(comp1)

# Run k-means with 3 initial clusters.
k3 <- kmeans(comp1, centers = 3, nstart = 25)
# Inspect model structure.
str(k3)
# Print model summary; between_SS/total_SS = 38.8%.
k3

# Find optimal k using the elbow method.
set.seed(100)

## Function to compute total within-cluster sum of squares.
wcss <- function(k) {
  kmeans(comp1, k, nstart = 10)$tot.withinss
}

# Compute and plot WCSS for k = 1 to k = 30.
k.values <- 1:30
# Apply WCSS to all k values.
wcss_k <- sapply(k.values, wcss)
plot(
  k.values,
  wcss_k,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

# Plot the graph using ggplot.
library(ggplot2)
# Create a data frame for ggplot.
elbow <- data.frame(k.values, wcss_k)
ggplot(elbow, aes(x = k.values, y = wcss_k)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 30, by = 1))

# Select the elbow point k = 7.
set.seed(100)
k7 <- kmeans(comp1, centers = 7, nstart = 10)
# Inspect k7 model structure.
str(k7)
# Print model summary; between_SS/total_SS = 63.1%. Higher ratio indicates more distinct clusters.
k7

# Re-run k = 3 for comparison (optional).
k3 <- kmeans(comp1, centers = 3, nstart = 10)
str(k3)
k3

# Summarize means at cluster level (using k = 7 clusters).
comp %>%
  select(price, speed, hd, ram, screen, ads, trend) %>%
  mutate(Cluster = k7$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

## Hierarchical clustering using complete linkage.
hc1 <- hclust(dist(comp1), method = "complete")
# Plot the obtained dendrogram.
plot(hc1, cex = 0.6, hang = -0.1)
# Draw rectangles with different colors for 7 clusters in the dendrogram.
# Argument border specifies the colors of the rectangles (default = 1 for black).
rect.hclust(hc1, k = 7, border = 2:8)