## Solution for Lab 8 task
# Set your work directory in which gradadmit.csv is saved.

# Load the graduate admissions dataset.
grad <- read.csv("gradadmit.csv", header = TRUE, sep = ",")
# Inspect dataset dimensions.
dim(grad)
# View summary statistics.
summary(grad)
# View structure (types) of each column.
str(grad)

# Convert variables gre and rank to numeric and normalize all numeric variables.
library(dplyr)
gd <- grad %>% mutate(gre = as.numeric(gre), rank = as.numeric(rank))

# Normalize the numeric variables using min-max scaling.
nor <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
gd[, 2:4] <- sapply(gd[, 2:4], nor)

# Convert outcome admit to factor.
gd$admit <- factor(gd$admit)
# Confirm structure after transformations.
str(gd)

# Split data into training and testing sets.
set.seed(100)
training.idx <- sample(1:nrow(gd), size = nrow(gd) * 0.8)
train.data <- gd[training.idx, ]
test.data <- gd[-training.idx, ]

## kNN classification
library(class)
set.seed(101)

# Try k = 2 first.
knn1 <- knn(train.data[, 2:4], test.data[, 2:4], cl = train.data$admit, k = 2)
# Compute accuracy for k = 2.
mean(knn1 == test.data$admit)
# 0.6625

# Find value of k for the best classifier.
ac <- rep(0, 30)
for (i in 1:30) {
  set.seed(101)
  knn.i <- knn(train.data[, 2:4], test.data[, 2:4], cl = train.data$admit, k = i)
  ac[i] <- mean(knn.i == test.data$admit)
  cat("k=", i, " accuracy=", ac[i], "\n")
}

# Accuracy plot.
plot(ac, type = "b", xlab = "K", ylab = "Accuracy")

# k = 1 results in highest accuracy. kNN correctly classifies 72.5% of the points
# in the test data set.
set.seed(101)
knn2 <- knn(train.data[, 2:4], test.data[, 2:4], cl = train.data$admit, k = 1)
mean(knn2 == test.data$admit)
# [1] 0.725

# Confusion matrix for kNN (k = 1).
knn_conf <- table(pred_knn2 = knn2, actual = test.data$admit)
knn_conf
#         actual
# pred_knn2  0  1
#        0 43 10
#        1 12 15

# Calculate false positive rate and recall for kNN.
knn_true_negative <- knn_conf["0", "0"]
knn_false_positive <- knn_conf["1", "0"]
knn_false_negative <- knn_conf["0", "1"]
knn_true_positive <- knn_conf["1", "1"]
knn_false_positive_rate <- knn_false_positive / (knn_false_positive + knn_true_negative)
knn_recall <- knn_true_positive / (knn_true_positive + knn_false_negative)
cat("kNN false positive rate:", knn_false_positive_rate, "\n")
cat("kNN recall (TPR):", knn_recall, "\n")

## Compare to logistic regression classification, where rank is used as a numeric
## variable (same as in kNN).
mlogit <- glm(admit ~ ., data = train.data, family = "binomial")
# View logistic regression summary.
summary(mlogit)

# Predicted probability P(Y=1).
Pred.p <- predict(mlogit, newdata = test.data, type = "response")
# Convert probabilities to predicted class labels.
y_pred_num <- ifelse(Pred.p > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels = c(0, 1))
# Accuracy for logistic regression classifier.
mean(y_pred == test.data$admit)
# 0.6875

# Confusion matrix for logistic regression.
glm_conf <- table(pred_glm = y_pred, actual = test.data$admit)
glm_conf
#        actual
# pred_glm  0  1
#       0 49 19
#       1  6  6

# Calculate false positive rate and recall for logistic regression.
glm_true_negative <- glm_conf["0", "0"]
glm_false_positive <- glm_conf["1", "0"]
glm_false_negative <- glm_conf["0", "1"]
glm_true_positive <- glm_conf["1", "1"]
glm_false_positive_rate <- glm_false_positive / (glm_false_positive + glm_true_negative)
glm_recall <- glm_true_positive / (glm_true_positive + glm_false_negative)
cat("GLM false positive rate:", glm_false_positive_rate, "\n")
cat("GLM recall (TPR):", glm_recall, "\n")

# Comment: The accuracy of kNN is 72.5%, slightly higher than the accuracy 68.8%
# of logistic regression classifier.
# In terms of misclassification rate, kNN yields a higher false positive rate but
# lower false negative rate than that in logistic regression.

# Note: false positive rate = no. of false positive / total no. of actual negative
#       = 12/(43+12) in kNN2 and 6/(49+6) in logistic regression.
# False negative rate = no. of false negative / total no. of actual positive
#       = 10/(10+15) in kNN2 and 19/(19+6) in logistic regression.

# Extra information: For two categorical variables x and y, function table(x, y)
# creates a table using x as row variable, y as column variable.