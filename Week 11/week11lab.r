library(dplyr)

# EXAM STEP 1: Load dataset
# Note: file in this repo is Week 11/computers.csv
pc <- read.csv("Week 11/computers.csv")

# EXAM STEP 2: Explore and prepare data
str(pc)
summary(pc)

# EXAM STEP 3: Remove missing values (if any)
pc <- pc[complete.cases(pc), ]

# EXAM STEP 4: Select numeric variables only
num_vars <- sapply(pc, is.numeric)
pc_num <- pc[, num_vars]

# EXAM STEP 5: Standardize numeric variables (required for k-means)
pc_scaled <- scale(pc_num)

# EXAM STEP 6: K-means with an initial K
set.seed(100)
k_initial <- kmeans(pc_scaled, centers = 3, nstart = 25)
k_initial

# EXAM STEP 7: Elbow method to find optimal K
set.seed(200)
k_max <- 10
wcss <- numeric(k_max)

for (k in 1:k_max) {
  km <- kmeans(pc_scaled, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

# Standard elbow curve
plot(1:k_max, wcss, type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "WCSS",
     main = "Elbow Method (WCSS)")

# 2nd difference of WCSS (discrete second derivative)
wcss_diff2 <- diff(wcss, differences = 2)
plot(3:k_max, wcss_diff2, type = "b", pch = 19, col = "blue",
     xlab = "k", ylab = "2nd difference of WCSS",
     main = "Elbow Method: 2nd WCSS Difference")

# Suggested elbow from largest curvature magnitude
elbow_k <- which.max(abs(wcss_diff2)) + 2
elbow_k

# EXAM STEP 8: K-means with optimal K (set after inspecting the elbow plot)
optimal_k <- 4
set.seed(100)
k_final <- kmeans(pc_scaled, centers = optimal_k, nstart = 25)
k_final

# EXAM STEP 9: Summarize clusters using original (unscaled) data
pc %>%
  mutate(Cluster = k_final$cluster) %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean))

# EXAM STEP 10: Hierarchical clustering + dendrogram
dist_matrix <- dist(pc_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "complete")
plot(hc, cex = 0.6, hang = -1)
rect.hclust(hc, k = optimal_k, border = 2:(optimal_k + 1))

