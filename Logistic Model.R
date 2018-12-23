## Load Libraries


## Step2: Import data

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
smp_size <- floor(0.75 * nrow(df_cleaned))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ] # randomly created training set
test <- df[-train_ind, ]# randomly created test set

nrow(train)
nrow(test)
ncol(train)
ncol(test)

# Step 5: Train the logistic regression model to predict income category

model = glm(formula = income ~ age + fnlwgt + education_num + hr_per_week,data = train,family = binomial(link = "logit"))
summary(model)

# Step 6: Predict on the test data 
income_predict = predict(model,test,type = "response")
head(income_predict,10)

## rep - replicates a value for given n number of times
income_predict_labels = rep("<=50K",nrow(test))
income_predict_labels[income_predict > 0.49]  = ">50K" 
head(income_predict_labels,10)
#plot(train$education,fitted(model))

# Step7: Construct Confusion Matrix, identify Misclassification Rate & Accuracy%

table(Preicted = income_predict_labels,Actual = test$income)
# Below code wil give missclassification Rate
mean(income_predict_labels != test$income)
# Below code will give Accuracy in predicting the income category
1- mean(income_predict_labels != test$income)
