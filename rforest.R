## Step1: Load libraries
library(e1071)
library(randomForest)

## Step 2: Import data
df = read.csv("Loan_data-dtrees.csv")

## Step3: Explore data
summary(df)
df$Principal <- as.factor(df$Principal)
df$terms  <- as.factor(df$terms)
summary(df[,c("Principal","terms")])

## Step3: Split data in training and testing
## 75% of the sample size
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproductible
set.seed(121)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ] # randomly created training set
test <- df[-train_ind, ]# randomly created test set

# Step4: Train the model

Formula = loan_status ~ Principal + terms + age + education + Gender
rforest <- randomForest(formula = Formula,data = train,proximity = T, ntree = 99 , maxnodes = 2, nodesize = 50)
summary(rforest)

##Step 5:  Predict on test data
predicted_values = predict(rforest,test,type = "class")

## Step 6: Confusion Matrix
table(predicted_values,test$loan_status)
# Below code wil give missclassification Rate
mean(predicted_values != test$loan_status)
# Below code will give Accuracy in predicting the loan repayment status
1- mean(predicted_values != test$loan_status)


