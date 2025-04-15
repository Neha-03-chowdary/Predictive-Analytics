# Load necessary libraries
library(dplyr)

# Load the dataset
getwd()
data <- read.csv("UCI_Credit_Card.csv")

# View the structure of the data
str(data)

# Convert necessary columns to factors
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$MARRIAGE <- as.factor(data$MARRIAGE)
data$default.payment.next.month <- as.factor(data$default.payment.next.month)

# Split the data into training and testing sets
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7, 0.3))
train <- data[sample, ]
test <- data[!sample, ]

# Fit a Logistic Regression model
model <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE +
               PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
               BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
               PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, 
             data = train, family = binomial)

# Summary of the model
summary(model)

# Predict on the test set
predictions <- predict(model, test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the model
confusion_matrix <- table(test$default.payment.next.month, predicted_classes)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))
