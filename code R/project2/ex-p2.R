house_price <- read.csv("C:/Users/HP/Desktop/house_price.csv")
View(house_price)
attach(house_price)
df=data.frame(price,sqft_living,floors,condition,sqft_above,sqft_living15,stringsAsFactors = FALSE )
View(df)
print(summary(is.na(df)))
df  <-  na.omit(df)
print(summary(is.na(df)))
logDF <- log(df)
View(logDF)
library(psych)
describe(df)	              #describe original data frame 
describe(logDF)             #describe data frame after transformation
hist (df[,"price"])
hist (logDF[,"price"])
hist (df[,"sqft_living"])
hist (logDF[,"sqft_living"])
hist (df[,"floors"])
hist (logDF[,"floors"])
hist (df[,"condition"])
hist (logDF[,"condition"])
hist (df[,"sqft_above"])
hist (logDF[,"sqft_above"])
hist (df[,"sqft_living15"])
hist (logDF[,"sqft_living15"])
price = logDF[,"price"]
sl = logDF[,"sqft_living"]
floors = logDF[,"floors"]
condition = logDF[,"condition"]
sa = logDF[,"sqft_above"]
sl15 = logDF[,"sqft_living15"]
boxplot(price~sl)
boxplot(price~floors)
boxplot(price~condition)
boxplot(price~sa)
boxplot(price~sl15)
pairs(logDF,col="red",main ="Pairs plot of the whole graph")

linear_model <- lm(price ~ sqft_living + floors + condition + sqft_above +sqft_living15, data = logDF)
print(summary(linear_model))
sqft_living <- mean(logDF[,"sqft_living"])
floors <- 2
condition <- 3
sqft_above <- mean(logDF[,"sqft_above"])
sqft_living15<-mean(logDF[,"sqft_living15"])
dat = data.frame(sqft_living,floors,condition,sqft_above,sqft_living15)
dat
print(predict(linear_model,dat))

sqft_living <- max(logDF[,"sqft_living"])
floors <- 2
condition <- 3
sqft_above <- max(logDF[,"sqft_above"])
sqft_living15<-max(logDF[,"sqft_living15"])
dat = data.frame(sqft_living,floors,condition,sqft_above,sqft_living15)
View(dat)
print(predict(linear_model,dat))



