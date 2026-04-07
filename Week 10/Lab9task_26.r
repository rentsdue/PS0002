## Lab 9 task
## (0) Prepare data using rotterdam dataset in the package survival.
library(dplyr)
library(survival)
# help(rotterdam)

# Inspect the structure of the rotterdam dataset.
str(rotterdam)

# Filter patients with no death, select features, and clean data types.
cancer1 <- rotterdam %>%
  filter(death == 0) %>%
  select(age, size:er, recur) %>%
  mutate(recur = factor(recur), size = as.numeric(size))

# Confirm structure after filtering/cleaning.
str(cancer1)
# 1710 rows, 7 columns, all variables are numeric except recur.
# Or, alternatively:
# cancer1$recur <- factor(cancer1$recur)
# cancer1$size <- as.numeric(cancer1$size)

## (1) Logistic regression classification
# Split data into training and testing sets.
set.seed(100)
training.idx <- sample(1:nrow(cancer1), size = nrow(cancer1) * 0.8)
train.data <- cancer1[training.idx, ]
test.data <- cancer1[-training.idx, ]

# Fit logistic regression model.
mlogit <- glm(recur ~ ., data = train.data, family = "binomial")
# Display model summary.
summary(mlogit)
# Interpret effects of significant predictors via odds ratios.
exp(coef(mlogit))

## Two predictors (grade and nodes) are significant at level 0.05.
## Interpretation:
## A 1-unit increase in grade increases the odds (relative risk) of cancer relapse by 1.62 times
## or increases the relative risk of relapse by 62%.
## Adding each additional node, a patient's relative risk of cancer relapse increases by 1.1 times
## (or by 10%).

# Predicted probability P(Y=1) for test data.
Pred.p <- predict(mlogit, newdata = test.data, type = "response")
# Convert probabilities to predicted class labels (0/1).
y_pred_num <- ifelse(Pred.p > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels = c(0, 1))
# Compute accuracy.
mean(y_pred == test.data$recur)
# 0.745614

# Confusion matrix for logistic regression.
lr_conf <- table(y_pred, y_actual = test.data$recur)
lr_conf
##           y_actual
## y_pred   0   1
## 0      252  81
## 1        6   3

## Misclassification rates:
## false positive rate = 6/(252+6) = 2.3%
## false negative rate = 81/(81+3) = 96.4%

# Calculate false positive rate and recall for logistic regression.
lr_true_negative <- lr_conf["0", "0"]
lr_false_positive <- lr_conf["1", "0"]
lr_false_negative <- lr_conf["0", "1"]
lr_true_positive <- lr_conf["1", "1"]
lr_false_positive_rate <- lr_false_positive / (lr_false_positive + lr_true_negative)
lr_recall <- lr_true_positive / (lr_true_positive + lr_false_negative)
cat("LR false positive rate:", lr_false_positive_rate, "\n")
cat("LR recall (TPR):", lr_recall, "\n")

## (2) KNN classification
# Normalize numeric variables using min-max scaling.
nor <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply normalization to numeric predictors.
cancer2 <- cancer1 %>% mutate(across(age:er, nor))
# Variables age:er are numeric.

# Or using the following:
# numvar <- sapply(cancer1, is.numeric)
# cancer1[, numvar] <- sapply(cancer1[, numvar], nor)

## Split data.
set.seed(100)
training.idx <- sample(1:nrow(cancer2), size = nrow(cancer2) * 0.8)
train.data <- cancer2[training.idx, ]
test.data <- cancer2[-training.idx, ]

# Classification with kNN.
library(class)
ac <- rep(0, 30)
for (i in 1:30) {
  set.seed(101)
  knn.i <- knn(train.data[, 1:6], test.data[, 1:6], cl = train.data$recur, k = i)
  ac[i] <- mean(knn.i == test.data$recur)
  cat("k=", i, " accuracy=", ac[i], "\n")
}

# Accuracy plot.
plot(ac, type = "b", xlab = "K", ylab = "Accuracy", col = "red", main = "plot of accuracy")

# k = 25 results in highest accuracy. kNN correctly classifies 76% of the points in the test data set.
set.seed(101)
knn2 <- knn(train.data[, 1:6], test.data[, 1:6], cl = train.data$recur, k = 25)
mean(knn2 == test.data$recur)
## accuracy = 76% slightly higher than that (74.6%) of logistic regression.

# Confusion matrix for kNN (k = 25).
knn_conf <- table(pred = knn2, actual = test.data$recur)
knn_conf
## Confusion matrix, number of false positives decreases from 6 in logistic regression to 1.
##     actual
## pred   0   1
##   0 257  81
##   1   1   3

## Misclassification rates:
## false positive rate = 1/(257+1) = 0.4%
## false negative rate = 81/(81+3) = 96.4%

# Calculate false positive rate and recall for kNN.
knn_true_negative <- knn_conf["0", "0"]
knn_false_positive <- knn_conf["1", "0"]
knn_false_negative <- knn_conf["0", "1"]
knn_true_positive <- knn_conf["1", "1"]
knn_false_positive_rate <- knn_false_positive / (knn_false_positive + knn_true_negative)
knn_recall <- knn_true_positive / (knn_true_positive + knn_false_negative)
cat("kNN false positive rate:", knn_false_positive_rate, "\n")
cat("kNN recall (TPR):", knn_recall, "\n")

## (3) SVM classification
library(e1071)
# Fit a linear kernel SVM model.
m.svm <- svm(recur ~ ., data = train.data, kernel = "linear")
# Display SVM model summary.
summary(m.svm)

# Predict using the linear SVM model.
pred.svm <- predict(m.svm, newdata = test.data[, 1:6])

# Check accuracy.
mean(pred.svm == test.data$recur)
# 0.754386 (close to that of LR and KNN classification)

# Confusion matrix for linear SVM.
svm_conf <- table(predict = pred.svm, actual = test.data$recur)
svm_conf
##          actual
## predict    0   1
##       0 258  84
##       1   0   0

# Misclassification rates:
# It correctly classifies all actual 0 cases without misclassification.
# false positive rate = 0/(258) = 0, smaller than those of LR and kNN.
# All actual 1 cases are misclassified, false negative rate = 84/(84+0) = 100%
# larger than both LR and kNN.

# Calculate false positive rate and recall for linear SVM.
svm_true_negative <- svm_conf["0", "0"]
svm_false_positive <- svm_conf["1", "0"]
svm_false_negative <- svm_conf["0", "1"]
svm_true_positive <- svm_conf["1", "1"]
svm_false_positive_rate <- svm_false_positive / (svm_false_positive + svm_true_negative)
svm_recall <- svm_true_positive / (svm_true_positive + svm_false_negative)
cat("SVM (linear) false positive rate:", svm_false_positive_rate, "\n")
cat("SVM (linear) recall (TPR):", svm_recall, "\n")

# SVM with radial kernel and tune its parameters.
set.seed(123) # Set a seed for CV.
m.svm.tune1 <- tune.svm(
  recur ~ .,
  data = train.data,
  kernel = "radial",
  cost = 10^(-1:2),
  gamma = c(.1, .5, 1, 2)
)
# Show tuning results.
summary(m.svm.tune1)

# Classification performance for the best radial SVM.
best.svm <- m.svm.tune1$best.model
pred.svm.tune <- predict(best.svm, newdata = test.data[, 1:6])
mean(pred.svm.tune == test.data$recur)
## 0.745614, slightly worse than linear kernel SVM, same as LR.

# Confusion matrix for tuned radial SVM.
svm_radial_conf <- table(pred_svm = pred.svm.tune, actual = test.data$recur)
svm_radial_conf
##          actual
## pred_svm  0   1
##      0 254  83
##      1   4   1

# Misclassification rates:
# false positive rate = 4/(254+4) = 1.6%
# false negative rate = 83/(83+1) = 98.8%

# Calculate false positive rate and recall for radial SVM.
svm_radial_true_negative <- svm_radial_conf["0", "0"]
svm_radial_false_positive <- svm_radial_conf["1", "0"]
svm_radial_false_negative <- svm_radial_conf["0", "1"]
svm_radial_true_positive <- svm_radial_conf["1", "1"]
svm_radial_false_positive_rate <- svm_radial_false_positive /
  (svm_radial_false_positive + svm_radial_true_negative)
svm_radial_recall <- svm_radial_true_positive /
  (svm_radial_true_positive + svm_radial_false_negative)
cat("SVM (radial) false positive rate:", svm_radial_false_positive_rate, "\n")
cat("SVM (radial) recall (TPR):", svm_radial_recall, "\n")

## We can stop here.
## Or you may want to use sigmoid kernel.
set.seed(121)
m.svm.tune2 <- tune.svm(
  recur ~ .,
  data = train.data,
  kernel = "sigmoid",
  gamma = c(0.1, 0.5, 1, 2, 3, 4),
  coef0 = c(0.1, 0.5, 1, 2, 3, 4)
)
# Show tuning results for sigmoid kernel.
summary(m.svm.tune2)

# Performance for sigmoid kernel model.
best.svm <- m.svm.tune2$best.model
pred.svm.tune <- predict(best.svm, newdata = test.data[, 1:6])
sigmoid_conf <- table(pred = pred.svm.tune, actual = test.data$y)
sigmoid_conf
mean(pred.svm.tune == test.data$recur)
# Accuracy = 0.754386 the same as the linear kernel SVM.

# Calculate false positive rate and recall for sigmoid SVM.
sigmoid_true_negative <- sigmoid_conf["0", "0"]
sigmoid_false_positive <- sigmoid_conf["1", "0"]
sigmoid_false_negative <- sigmoid_conf["0", "1"]
sigmoid_true_positive <- sigmoid_conf["1", "1"]
sigmoid_false_positive_rate <- sigmoid_false_positive /
  (sigmoid_false_positive + sigmoid_true_negative)
sigmoid_recall <- sigmoid_true_positive /
  (sigmoid_true_positive + sigmoid_false_negative)
cat("SVM (sigmoid) false positive rate:", sigmoid_false_positive_rate, "\n")
cat("SVM (sigmoid) recall (TPR):", sigmoid_recall, "\n")

# Therefore, we use the linear kernel SVM to compare with LR and kNN.

## (4) Comparison between LR, kNN and SVM
# In terms of accuracy, kNN with k = 25 produces the highest accuracy of 76%,
# while the other methods perform similarly well with accuracy around 75%.
# We can choose kNN for this classification problem.

## However, 3 methods produce different false positive/negative rates.
# For a patient, false negative leads to missed diagnosis for the cancer return (relapse),
# while false positive results in wasted time or resources.
# So, a small false negative rate is more desirable than a small false positive rate.
# In terms of false negative rate and overall accuracy, kNN with k = 25 classification
# performs the best. LR gives the same false negative rate with slightly lower accuracy,
# but has interpretability.

########################################################################
## Alternative way.
## The normalization step can be done simply before logistic regression.
## All LR, kNN and SVM methods use the normalized data.
## Classification results will be the same as those given above.
## However, when interpreting predictors' effects in logistic regression,
## the original unit of each predictor changes in this case.
########################################################################