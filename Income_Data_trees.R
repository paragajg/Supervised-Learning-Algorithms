## Step1: Load libraries
library(e1071)
library(rpart) # decision trees # trees
library(ggplot2) ## Graphical plots
library(RColorBrewer)
library(rattle)# required for prp() function
library(rpart.plot)

## Step 2 Import data

df = read.csv("train.csv.xls")
validation = read.csv("test.csv.xls")

## Step3: Preprocessing of data
summary(df)
summary(validation)

# Removing rows containing "?" as a value

df_cleaned = subset(x = df, workclass !="?" & native_country !="?")
## workclass ! = '?' means is not equal to '?' should be selected
validation_cleaned = subset(x = validation,workclass !="?" & native_country!="?")


# Step 4: Create data partition for training & test data

## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ] # randomly created training set
test <- df[-train_ind, ]# randomly created test set

nrow(train)
nrow(test)



tree <- rpart(formula = income ~ age + fnlwgt + education_num + hr_per_week,data = train, method = "class",control = rpart.control(maxdepth = 10,cp = 0.01))

summary(tree)

## Plot Tree
par(xpd = TRUE)
#rattle()
prp(tree,main = "Decision Tree Visualization",type=2, extra=104,cex=.75)
fancyRpartPlot(tree,main = "Decision Tree Model",cex.main=0.75)


##Step 5:  Predict on test data
predicted_values = predict(tree,test,type = "class")

## Step 6: Confusion Matrix
table(Predictions = predicted_values,Acutal = test$income)
# Below code wil give missclassification Rate
mean(predicted_values != test$income)
# Below code will give Accuracy in predicting the loan repayment status
1- mean(predicted_values != test$income)
