# Step1: Load Libraries
library(class)

# Step2: Import data
df = read.csv("train.csv.xls")
validation = read.csv("test.csv.xls")

## Step3: Preprocessing of data
summary(df)
summary(validation)

# Removing rows containing "?" as a value
df_cleaned = subset(x = df, workclass !="?" & native_country !="?")
validation_cleaned = subset(x = validation,workclass !="?" & native_country!="?")

summary(df_cleaned)

## Note: knn algorithm may get affected by the different scales of numerica features. If variable is measured in km and other in mm then the feature with mm may influence the outcome of the model which will be incorrect. Hence all the features should be scaled to have similar units.

?scale
df_cleaned1 = data.frame(scale(df_cleaned[,c(3,5,7)]))
df_cleaned1$income = df_cleaned$income

# Step 4: Create data partition for training & test data

## 75% of the sample size
smp_size <- floor(0.75 * nrow(df_cleaned1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_cleaned1)), size = smp_size)

train <- df_cleaned1[train_ind, ] # randomly created training set
test <- df_cleaned1[-train_ind, ]# randomly created test set

nrow(train)
nrow(test)


# Step4: Create knn model
income_predict_knn = knn(train[,-4],test[,-4],cl = train$income,k = 5)

# Step5: Create Confusion Matrix, Misscloassification rate & Accuracy%
table(income_predict_knn,test$income)

mean(income_predict_knn != test$income)
# Below code will give Accuracy in predicting the income category
1- mean(income_predict_knn != test$income)
