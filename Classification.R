## Load Libraries
library(MASS)
library(lubridate)
library(chron)
library(plyr)

## Step2: Import data

df = read.csv("trains_train.csv")
validation = read.csv("trains_test.csv")

#combined_data <- rbind(df[,-7],validation) 

## Step3: Preprocessing of data
summary(df)
summary(validation)

#test = strsplit(as.character(df$time),":")
#test[2]
test = as.character(paste(df$date,df$time))
test = strptime(test,"%Y-%m-%d %I:%M:%S %p")

test1 = hours(test)

daystatus <- ifelse(test1 > 6 & test1 <= 12,"Morning",
                    ifelse(test1 > 12 & test1 <= 19,"Afternoon","Night"))


test1 = hours(test)
weekday = weekdays(test)
df$weekday = as.factor(weekday)
df$daytime = as.factor(daystatus)

freq = count(df$vehicle)
freqgroup <- function(x) {
  if(x <=3){
    Freq = "Low"
  }if(x >3 & x < 10){
    Freq = "Medium"
  }else{
    Freq = "High"
    }
}

temp = ifelse(freq$freq <=3, "Low",
              ifelse(freq$freq >3 & freq$freq <=10, "Medium","High")
              )

Freq_vehicles = unlist(lapply(X = freq$freq,function(x) freqgroup(x)))

freq$demand = temp

df_new = merge(df,freq,by.x = "vehicle",by.y = "x")

## Validation
#test = strsplit(as.character(df$time),":")
#test[2]
test = as.character(paste(validation$date,validation$time))
test = strptime(test,"%Y-%m-%d %I:%M:%S %p")


test1 = hours(test)

daystatus <- ifelse(test1 > 6 & test1 <= 12,"Morning",
                    ifelse(test1 > 12 & test1 <= 19,"Afternoon","Night"))

weekday = weekdays(test)
validation$weekday = as.factor(weekday)
validation$daytime = as.factor(daystatus)

freq = count(validation$vehicle)
temp = ifelse(freq$freq <=3, "Low",
              ifelse(freq$freq >3 & freq$freq <=10, "Medium","High")
)

#Freq_vehicles = unlist(lapply(X = freq$freq,function(x) freqgroup(x)))
freq$demand = temp
validaiton_new = merge(validation,freq,by.x = "vehicle",by.y = "x")

# Step 4: Create data partition for training & test data

## 75% of the sample size
# Step4: Train the model
df_new$demand = as.factor(df_new$demand)
Formula = occupancy ~ weekday + daytime + demand + from + to
tree <- rpart(formula = Formula,data = df_new,method = "class",control = rpart.control(maxdepth = 5,cp = 0.001))
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
predicted_values = predict(tree,validaiton_new,type = "class")

validaiton_new$prediction = predicted_values

prediction = ifelse(validaiton_new$prediction =="low",0,
                    ifelse(validaiton_new =="medium",1,2))
validaiton_new$Prediction = prediction

write.csv(x = validaiton_new,"predictions.csv")

##### Random Forest
df_new$demand = as.factor(df_new$demand)
Formula = occupancy ~ weekday + daytime + demand
rforest <- randomForest(formula = Formula,data = df_new,proximity = T, ntree = 500)

validaiton_new$demand = as.factor(validaiton_new$demand)
predicted_values = predict(rforest,validaiton_new,type = "class")

validaiton_new$prediction = predicted_values

prediction = ifelse(validaiton_new$prediction =="low",0,
                    ifelse(validaiton_new =="medium",1,2))
validaiton_new$Prediction = prediction

write.csv(x = validaiton_new,"predictions_rf.csv")
