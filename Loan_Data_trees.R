## Step1: Load libraries
library(e1071)
library(rpart) # decision trees # trees
library(ggplot2) ## Graphical plots
library(RColorBrewer)
library(rattle)# required for prp() function
library(rpart.plot)

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
tree <- rpart(formula = Formula,data =train, method = "class",control = rpart.control(maxdepth = 10,cp = 0.001))
summary(tree)
## Plotting of trees - basic plotting
plotcp(tree,cex.lab=1.5,cex.axis=1.5)
#warnings()
par(xpd = TRUE)
plot(tree)
text(tree,splits = T,use.n=T,all=F,fancy=T,bg=par("bg"),minlength=1L)



par(xpd = TRUE)
#rattle()
prp(tree,main = "Decision Tree Visualization",type=2, extra=104,cex=.75)
fancyRpartPlot(tree,main = "Decision Tree Model",cex.main=0.75)

##Step 5:  Predict on test data
predicted_values = predict(tree,test,type = "class")

## Step 6: Confusion Matrix
table(predicted_values,test$loan_status)
# Below code wil give missclassification Rate
mean(predicted_values != test$loan_status)
# Below code will give Accuracy in predicting the loan repayment status
1- mean(predicted_values != test$loan_status)



