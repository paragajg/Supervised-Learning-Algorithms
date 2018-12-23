library(e1071)
library(rpart) # decision trees # trees
library(ggplot2) ## Graphical plots
library(RColorBrewer)
#library(rattle)# required for prp() function
library(rpart.plot)
library(randomForest)

tree <- rpart(formula = Churn ~.,data = training, method = "class")

summary(tree)

## Plot Tree
par(xpd = TRUE)
#rattle()
prp(tree,main = "Decision Tree Visualization",type=2, extra=104,cex=.75)
#fancyRpartPlot(tree,main = "Decision Tree Model",cex.main=0.75)


##Step 5:  Predict on test data
predicted_values = predict(tree,testing,type = "class")

## Step 6: Confusion Matrix

print("Confusion Matrix for Decision Tree"); table(Predicted = predicted_values, Actual = testing$Churn)
# Below code will give Accuracy in predicting the loan repayment status
testing$Churn <- ifelse(testing$Churn == '1','Yes','No')
misClasificError <- mean(predicted_values != testing$Churn)
1- misClasificError


## Construct Random Forest
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

# Confusion Matrix
pred_rf <- predict(rfModel, testing)
confusionMatrix(pred_rf, testing$Churn)

varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


## Key Insights
"
Features such as tenure_group, Contract, PaperlessBilling, MonthlyCharges and InternetService appear to play a role in customer churn.

There does not seem to be a relationship between gender and churn.

Customers in a month-to-month contract, with No DSL internet Connection and are within 12 months tenure, are more likely to churn; On the other hand, customers with one or two year contract, with longer than 12 months tenure, that are not using PaperlessBilling, are less likely to churn.
"