library(dplyr) # Load dplyr for piping and data summarization helpers.

# EXAM STEP 1: Load dataset
# Note: file in this repo is Week 11/computers.csv
pc <- read.csv("Week 11/computers.csv") # Read computers.csv into data frame pc.

# EXAM STEP 2: Explore and prepare data
str(pc) # Inspect column types and overall structure.
summary(pc) # Inspect summary statistics and distributions.

# EXAM STEP 3: Remove missing values (if any)
pc <- pc[complete.cases(pc), ] # Keep only rows with complete data.

# EXAM STEP 4: Select numeric variables only
num_vars <- sapply(pc, is.numeric) # Identify which columns are numeric.
pc_num <- pc[, num_vars] # Subset dataset to numeric columns only.

# EXAM STEP 5: Standardize numeric variables (required for k-means)
pc_scaled <- scale(pc_num) # Standardize numeric features to mean 0 and sd 1.

# EXAM STEP 6: K-means with an initial K
set.seed(100) # Set random seed for reproducible k-means initialization.
k_initial <- kmeans(pc_scaled, centers = 3, nstart = 25) # Run initial k-means with 3 clusters.
k_initial # Print initial k-means clustering result.

# EXAM STEP 7: Elbow method to find optimal K
set.seed(200) # Set seed before repeated k-means runs for elbow analysis.
k_max <- 10 # Define maximum number of clusters to evaluate.
wcss <- numeric(k_max) # Initialize vector to store WCSS for each k.

for (k in 1:k_max) { # Loop from 1 to k_max clusters.
  km <- kmeans(pc_scaled, centers = k, nstart = 25) # Fit k-means model for current k.
  wcss[k] <- km$tot.withinss # Store total within-cluster sum of squares.
} # End elbow loop.

# Standard elbow curve
plot(1:k_max, wcss, type = "b", pch = 19, # Plot WCSS against number of clusters.
     xlab = "Number of clusters (k)", ylab = "WCSS", # Label axes for elbow chart.
     main = "Elbow Method (WCSS)") # Add chart title.

# 2nd difference of WCSS (discrete second derivative)
wcss_diff2 <- diff(wcss, differences = 2) # Compute second differences to detect elbow curvature.
plot(3:k_max, wcss_diff2, type = "b", pch = 19, col = "blue", # Plot second differences for k values 3..k_max.
     xlab = "k", ylab = "2nd difference of WCSS", # Label axes for second-difference plot.
     main = "Elbow Method: 2nd WCSS Difference") # Add chart title.

# Suggested elbow from largest curvature magnitude
elbow_k <- which.max(abs(wcss_diff2)) + 2 # Estimate elbow k from largest absolute second difference.
elbow_k # Print suggested elbow value.

# EXAM STEP 8: K-means with optimal K (set after inspecting the elbow plot)
optimal_k <- 4 # Set chosen final number of clusters.
set.seed(100) # Set seed for reproducible final clustering.
k_final <- kmeans(pc_scaled, centers = optimal_k, nstart = 25) # Fit final k-means model with chosen k.
k_final # Print final k-means output.

# EXAM STEP 9: Summarize clusters using original (unscaled) data
pc %>%
  mutate(Cluster = k_final$cluster) %>% # Append cluster labels to original dataset.
  group_by(Cluster) %>% # Group rows by assigned cluster.
  summarise(across(where(is.numeric), mean)) # Compute mean of each numeric variable per cluster.

# EXAM STEP 10: Hierarchical clustering + dendrogram
dist_matrix <- dist(pc_scaled, method = "euclidean") # Compute Euclidean distance matrix on scaled data.
hc <- hclust(dist_matrix, method = "complete") # Perform hierarchical clustering with complete linkage.
plot(hc, cex = 0.6, hang = -1) # Draw dendrogram with adjusted label size and branch hang.
rect.hclust(hc, k = optimal_k, border = 2:(optimal_k + 1)) # Draw rectangles showing final cluster cut.


