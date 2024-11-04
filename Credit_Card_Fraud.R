cardTransact<-card_transdata
str(cardTransact)
# Calculate the quartiles and IQR
quartiles <- quantile(cardTransact$ratio_to_median_purchase_price, probs = c(0.25, 0.5, 0.75))
IQR <- quartiles[3] - quartiles[1]

# Calculate the lower and upper bounds for outliers
lowerBound <- quartiles[1] - 1.5 * IQR
upperBound <- quartiles[3] + 1.5 * IQR

boxplot(filteredData$distance_from_home, main = 'Box Plot', xlab = 'Distance from home', ylab = 'km', notch = TRUE, col = 'red')

boxplot(filteredData$distance_from_last_transaction, main = 'Box Plot', xlab = 'Distance from last Transaction', ylab = 'km', notch = TRUE, col = 'red')

# Create the box plot without outliers
boxplot(filteredData$ratio_to_median_purchase_price, main = 'Box Plot', xlab = 'Ratio to median purchase price', ylab = 'km', notch = TRUE, col = 'red')
# Filter the data to remove outliers 
card_transdata <- card_transdata[card_transdata$ratio_to_median_purchase_price >= lowerBound & card_transdata$ratio_to_median_purchase_price <= upperBound, ] 
#split the data 
set.seed(345) 
train=sample(1:nrow(card_transdata),nrow(card_transdata)*(1/10))
train
#use the train index set to split the dataset 
card_transdata.train=card_transdata[train,] 
card_transdata.test=card_transdata[-train,]
#display basic results 
fit=rpart(fraud~.,
         data=card_transdata.train,
         method="class",
         control=rpart.control(xval=50,minsplit=1000),
         parms=list(split="gini"))
fit
#tree interpretation 
rpart.plot(fit, type = 1, extra = 1)
#extract the vector of predicted class for each observation in train data 
card.pred <- predict(fit, card_transdata.train, type="class")
#extract the actual class of each observation in train data 
card.actual <- card_transdata.train$fraud 
#confusion matrix for train data 
conf_matrix <- table(card.pred,card.actual) 
conf_matrix 
#parameters
# Extracting values from confusion matrix
TP <- conf_matrix[2, 2]  # True Positives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives
TN <- conf_matrix[1, 1]  # True Negatives

# Calculating accuracy
accuracy <- (TP + TN) / sum(conf_matrix)
accuracy
# Calculating true positive rate (TPR) or sensitivity
TPR <- TP / (TP + FN)
TPR
# Calculating error rate
error_rate <- 1 - accuracy
error_rate
# Calculating false negative rate (FNR)
FNR <- FN / (FN + TP)
FNR
# Calculating true negative rate (TNR) or specificity
TNR <- TN / (TN + FP)
TNR
# Calculating false positive rate (FPR)
FPR <- FP / (FP + TN)
FPR
# Printing the results
print(paste("Accuracy:", accuracy))
print(paste("True Positive Rate (TPR):", TPR))
print(paste("Error Rate:", error_rate))
print(paste("False Negative Rate (FNR):", FNR))
print(paste("True Negative Rate (TNR):", TNR))
print(paste("False Positive Rate (FPR):", FPR))


#extract the vector of predicted class for each observation in test data 
card.predt <- predict(fit, card_transdata.test, type="class") 
#extract the actual class of each observation in test data 
card.actualt <- card_transdata.test$fraud 
#confusion matrix for test data 
conf_matrix <- table(card.predt,card.actualt) 
conf_matrix
#parameters
# Extracting values from confusion matrix
TP <- conf_matrix[2, 2]  # True Positives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives
TN <- conf_matrix[1, 1]  # True Negatives

# Calculating accuracy
accuracy <- (TP + TN) / sum(conf_matrix)

# Calculating true positive rate (TPR) or sensitivity
TPR <- TP / (TP + FN)

# Calculating error rate
error_rate <- 1 - accuracy

# Calculating false negative rate (FNR)
FNR <- FN / (FN + TP)

# Calculating true negative rate (TNR) or specificity
TNR <- TN / (TN + FP)

# Calculating false positive rate (FPR)
FPR <- FP / (FP + TN)

# Printing the results
print(paste("Accuracy:", accuracy))
print(paste("True Positive Rate (TPR):", TPR))
print(paste("Error Rate:", error_rate))
print(paste("False Negative Rate (FNR):", FNR))
print(paste("True Negative Rate (TNR):", TNR))
print(paste("False Positive Rate (FPR):", FPR))


card_transdata$fraud<- as.factor(card_transdata$fraud)
train<-sample(1:nrow(card_transdata),(0.7)*nrow(card_transdata))
train.df<-card_transdata[train,]
test.df<-card_transdata[-train,]
logit.reg<-glm(fraud ~ .,data=train.df,family="binomial")
summary(logit.reg)

# Convert the fraud column to a factor
train.df$fraud <- as.factor(train.df$fraud)

# Make predictions on the test dataset
train.df$predicted_fraud <- predict(logit.reg, newdata = train.df, type = "response")
train.df$predicted_fraud_class <- factor(ifelse(train.df$predicted_fraud > 0.5, 1, 0), levels = c("0", "1"))

# Compute the confusion matrix and accuracy
confusion_matrix <- confusionMatrix(train.df$predicted_fraud_class, train.df$fraud)
confusion_matrix
accuracy <- confusion_matrix$overall['Accuracy']
accuracy


test.df$fraud <- as.factor(test.df$fraud)

# Make predictions on the test dataset
test.df$predicted_fraud <- predict(logit.reg, newdata = test.df, type = "response")
test.df$predicted_fraud_class <- factor(ifelse(test.df$predicted_fraud > 0.5, 1, 0), levels = c("0", "1"))

# Compute the confusion matrix and accuracy
confusion_matrix <- confusionMatrix(test.df$predicted_fraud_class, test.df$fraud)
confusion_matrix
accuracy <- confusion_matrix$overall['Accuracy']
accuracy










