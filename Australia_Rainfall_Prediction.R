library(tidyverse)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(caret)
library(MASS)
library(lmtest)
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }

#1
df <- read.csv("weatherAUS.csv")
df$RainToday <- NULL
df$RainTomorrow <- NULL
df$RISK_MM <- NULL

#2
sapply(df, class)
df$Date <- as.Date(df$Date , format = "%Y-%m-%d")
df$Cloud9am <- as.factor(df$Cloud9am)
df$Cloud3pm <- as.factor(df$Cloud3pm)
sapply(df, class)

#3
summary(df)
tail(sort(df$Rainfall))
tail(sort(df$WindGustSpeed))
tail(sort(df$WindSpeed9am))
tail(sort(df$WindSpeed3pm))
if (FALSE){
  outliers1<-boxplot(df$MinTemp)$out
  if(!is_empty(outliers1)) df<-df[-which(df$MinTemp %in% outliers1),]
  outliers2<-boxplot(df$MaxTemp)$out
  if(!is_empty(outliers2)) df<-df[-which(df$MaxTemp %in% outliers2),]
  outliers3<-boxplot(df$Rainfall)$out
  if(!is_empty(outliers3)) df<-df[-which(df$Rainfall %in% outliers3),]
  outliers4<-boxplot(df$WindGustSpeed)$out
  if(!is_empty(outliers4)) df<-df[-which(df$WindGustSpeed %in% outliers4),]
  outliers5<-boxplot(df$WindSpeed9am)$out
  if(!is_empty(outliers5)) df<-df[-which(df$WindSpeed9am %in% outliers5),]
  outliers6<-boxplot(df$WindSpeed3pm)$out
  if(!is_empty(outliers6)) df<-df[-which(df$WindSpeed3pm %in% outliers6),]
  outliers7<-boxplot(df$Humidity9am)$out
  if(!is_empty(outliers7)) df<-df[-which(df$Humidity9am %in% outliers7),]
  outliers8<-boxplot(df$Humidity3pm)$out
  if(!is_empty(outliers8)) df<-df[-which(df$Humidity3pm %in% outliers8),]
  outliers9<-boxplot(df$Pressure9am)$out
  if(!is_empty(outliers9)) df<-df[-which(df$Pressure9am %in% outliers9),]
  outliers10<-boxplot(df$Pressure3pm)$out
  if(!is_empty(outliers10)) df<-df[-which(df$Pressure3pm %in% outliers10),]
  outliers11<-boxplot(df$Temp9am)$out
  if(!is_empty(outliers11)) df<-df[-which(df$Temp9am %in% outliers11),]
  outliers12<-boxplot(df$Temp3pm)$out
  if(!is_empty(outliers12)) df<-df[-which(df$Temp3pm %in% outliers12),]
  summary(df)
}

#4
sum(is.na(df))
sum(is.na(df$Evaporation)) / nrow(df)
sum(is.na(df$Sunshine)) / nrow(df)
sum(is.na(df$Cloud9am)) / nrow(df)
sum(is.na(df$Cloud3pm)) / nrow(df)
df$Evaporation <- NULL
df$Sunshine <- NULL
df$WindGustDir <- as.character(df$WindGustDir)
df$WindGustDir <- ifelse(is.na(df$WindGustDir), 'missing', df$WindGustDir)
df$WindGustDir <- as.factor(df$WindGustDir)
df$WindDir9am <- as.character(df$WindDir9am)
df$WindDir9am <- ifelse(is.na(df$WindDir9am), 'missing', df$WindDir9am)
df$WindDir9am <- as.factor(df$WindDir9am)
df$WindDir3pm <- as.character(df$WindDir3pm)
df$WindDir3pm <- ifelse(is.na(df$WindDir3pm), 'missing', df$WindDir3pm)
df$WindDir3pm <- as.factor(df$WindDir3pm)
df$Cloud9am <- as.character(df$Cloud9am)
df$Cloud9am <- ifelse(is.na(df$Cloud9am), 'missing', df$Cloud9am)
df$Cloud9am <- as.factor(df$Cloud9am)
df$Cloud3pm <- as.character(df$Cloud3pm)
df$Cloud3pm <- ifelse(is.na(df$Cloud3pm), 'missing', df$Cloud3pm)
df$Cloud3pm <- as.factor(df$Cloud3pm)
df$MinTemp[which(is.na(df$MinTemp))] <- median(df$MinTemp,na.rm = TRUE)
df$MaxTemp[which(is.na(df$MaxTemp))] <- median(df$MaxTemp,na.rm = TRUE)
df$Rainfall[which(is.na(df$Rainfall))] <- median(df$Rainfall,na.rm = TRUE)
df$WindGustSpeed[which(is.na(df$WindGustSpeed))] <- median(df$WindGustSpeed,na.rm = TRUE)
df$WindSpeed9am[which(is.na(df$WindSpeed9am))] <- median(df$WindSpeed9am,na.rm = TRUE)
df$WindSpeed3pm[which(is.na(df$WindSpeed3pm))] <- median(df$WindSpeed3pm,na.rm = TRUE)
df$Humidity9am[which(is.na(df$Humidity9am))] <- mean(df$Humidity9am,na.rm = TRUE)
df$Humidity3pm[which(is.na(df$Humidity3pm))] <- mean(df$Humidity3pm,na.rm = TRUE)
df$Pressure9am[which(is.na(df$Pressure9am))] <- median(df$Pressure9am,na.rm = TRUE)
df$Pressure3pm[which(is.na(df$Pressure3pm))] <- median(df$Pressure3pm,na.rm = TRUE)
df$Temp9am[which(is.na(df$Temp9am))] <- median(df$Temp9am,na.rm = TRUE)
df$Temp3pm[which(is.na(df$Temp3pm))] <- median(df$Temp3pm,na.rm = TRUE)


#5
ggplot(df, aes(Location)) + geom_bar() + coord_flip()

#6
highestMax <- df$Location[which.max(df$MaxTemp)]
highestMin <- df$Location[which.max(df$MinTemp)]
highestRain <- df$Location[which.max(df$Rainfall)]
subset(df, MaxTemp==max(df$MaxTemp))
subset(df, MinTemp==max(df$MinTemp))
subset(df, Rainfall==max(df$Rainfall))

#7
my_data <- df[, c(3,4,5,7,10,11,12,13,14,15,18,19)]
res <- rcorr(as.matrix(my_data))
corrplot(res$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res$r, tl.col = "black", type = "upper", tl.srt = 45)

#8
ggplot(df, aes(x=WindSpeed3pm, y=WindSpeed9am)) + geom_point()
ggplot(df, aes(x=Humidity9am , y=WindSpeed9am)) + geom_point()
ggplot(df, aes(x=WindSpeed3pm, y=WindSpeed9am, color=WindDir9am)) + geom_point()
ggplot(df, aes(x=Humidity9am , y=WindSpeed9am, color=WindDir9am)) + geom_point()

#9
boxplot(df$MinTemp, df$MaxTemp, names = c("MinTemp","MaxTemp"))
df$Location[which.max(df$MaxTemp)]
df$Location[which.min(df$MinTemp)]
dfMod <- subset(df, Location %in% c("Woomera","MountGinini"))
dfMod <- as.data.frame(dfMod[, c(2,5)])
ggplot(dfMod, aes(x=Location, y=Rainfall)) + geom_boxplot()

#10
#a
in_train <- createDataPartition(df$Rainfall, p = 3/4, list = FALSE, times = 1)
train_data <- df[ in_train,]
test_data  <- df[-in_train,]
summary(train_data)
summary(test_data)

#b
lm1 <- lm(Rainfall ~ Pressure9am, data=train_data)

#c
summary(lm1)
lm1$coefficients

#d
test_data$newlm1 <- predict.lm(lm1, test_data)
rmse(test_data$Rainfall,test_data$newlm1)
rsq(test_data$Rainfall,test_data$newlm1)
if(FALSE){
  lm1c <- train(Rainfall ~ Pressure9am, data=train_data, method = "lm")
  summary(lm1c)
  test_data$newlm1c <- predict (lm1c, test_data)
  rmse(test_data$Rainfall,test_data$newlm1c)
  rsq(test_data$Rainfall,test_data$newlm1c)
}

#e
lm2 <- train(Rainfall ~ MinTemp, data=train_data, method = "lm")
summary(lm2)
test_data$newlm2 <- predict (lm2, test_data)
rmse(test_data$Rainfall,test_data$newlm2)
rsq(test_data$Rainfall,test_data$newlm2)

lm3 <- train(Rainfall ~ MaxTemp, data=train_data, method = "lm")
summary(lm3)
test_data$newlm3 <- predict (lm3, test_data)
rmse(test_data$Rainfall,test_data$newlm3)
rsq(test_data$Rainfall,test_data$newlm3)

lm4 <- train(Rainfall ~ WindGustDir, data=train_data, method = "lm")
summary(lm4)
test_data$newlm4 <- predict (lm4, test_data)
rmse(test_data$Rainfall,test_data$newlm4)
rsq(test_data$Rainfall,test_data$newlm4)

lm5 <- train(Rainfall ~ WindGustSpeed, data=train_data, method = "lm")
summary(lm5)
test_data$newlm5 <- predict (lm5, test_data)
rmse(test_data$Rainfall,test_data$newlm5)
rsq(test_data$Rainfall,test_data$newlm5)

lm6 <- train(Rainfall ~ WindDir9am, data=train_data, method = "lm")
summary(lm6)
test_data$newlm6 <- predict (lm6, test_data)
rmse(test_data$Rainfall,test_data$newlm6)
rsq(test_data$Rainfall,test_data$newlm6)

lm7 <- train(Rainfall ~ WindDir3pm, data=train_data, method = "lm")
summary(lm7)
test_data$newlm7 <- predict (lm7, test_data)
rmse(test_data$Rainfall,test_data$newlm7)
rsq(test_data$Rainfall,test_data$newlm7)

lm8 <- train(Rainfall ~ WindSpeed9am, data=train_data, method = "lm")
summary(lm8)
test_data$newlm8 <- predict (lm8, test_data)
rmse(test_data$Rainfall,test_data$newlm8)
rsq(test_data$Rainfall,test_data$newlm8)

lm9 <- train(Rainfall ~ WindSpeed3pm, data=train_data, method = "lm")
summary(lm9)
test_data$newlm9 <- predict (lm9, test_data)
rmse(test_data$Rainfall,test_data$newlm9)
rsq(test_data$Rainfall,test_data$newlm9)

lm10 <- train(Rainfall ~ Humidity9am, data=train_data, method = "lm")
summary(lm10)
test_data$newlm10 <- predict (lm10, test_data)
rmse(test_data$Rainfall,test_data$newlm10)
rsq(test_data$Rainfall,test_data$newlm10)

lm11 <- train(Rainfall ~ Humidity3pm, data=train_data, method = "lm")
summary(lm11)
test_data$newlm11 <- predict (lm11, test_data)
rmse(test_data$Rainfall,test_data$newlm11)
rsq(test_data$Rainfall,test_data$newlm11)

lm12 <- train(Rainfall ~ Pressure3pm, data=train_data, method = "lm")
summary(lm12)
test_data$newlm12 <- predict (lm12, test_data)
rmse(test_data$Rainfall,test_data$newlm12)
rsq(test_data$Rainfall,test_data$newlm12)

lm13 <- train(Rainfall ~ Cloud9am, data=train_data, method = "lm")
summary(lm13)
test_data$newlm13 <- predict (lm13, test_data)
rmse(test_data$Rainfall,test_data$newlm13)
rsq(test_data$Rainfall,test_data$newlm13)

lm14 <- train(Rainfall ~ Cloud3pm, data=train_data, method = "lm")
summary(lm14)
test_data$newlm14 <- predict (lm14, test_data)
rmse(test_data$Rainfall,test_data$newlm14)
rsq(test_data$Rainfall,test_data$newlm14)

lm15 <- train(Rainfall ~ Temp9am, data=train_data, method = "lm")
summary(lm15)
test_data$newlm15 <- predict (lm15, test_data)
rmse(test_data$Rainfall,test_data$newlm15)
rsq(test_data$Rainfall,test_data$newlm15)

lm16 <- train(Rainfall ~ Temp3pm, data=train_data, method = "lm")
summary(lm16)
test_data$newlm16 <- predict (lm16, test_data)
rmse(test_data$Rainfall,test_data$newlm16)
rsq(test_data$Rainfall,test_data$newlm16)

lm17 <- train(Rainfall ~ Temp9am + Pressure3pm + WindGustDir + WindGustSpeed + WindDir9am + WindDir3pm + WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm + Cloud9am + Cloud3pm, data=train_data, method = "lm")
summary(lm17)
test_data$newlm17 <- predict (lm17, test_data)
rmse(test_data$Rainfall,test_data$newlm17)
rsq(test_data$Rainfall,test_data$newlm17)

ggplot(data=test_data,aes(x=newlm17,y=newlm17-Rainfall)) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=newlm17,y=newlm17-Rainfall),color="black")

lm18 <- train(Rainfall ~ Temp9am + Pressure3pm + Location + Date, data=train_data, method = "lm")
summary(lm18)
test_data$newlm18 <- predict (lm18, test_data)
rmse(test_data$Rainfall,test_data$newlm18)
rsq(test_data$Rainfall,test_data$newlm18)

#f
lm19 <- lm(Rainfall ~., data = train_data)
lm19bw <- stepAIC(lm19, direction = "backward", trace = FALSE)
summary(lm19bw)
test_data$newlm19 <- predict (lm19bw, test_data)
rmse(test_data$Rainfall,test_data$newlm19)
rsq(test_data$Rainfall,test_data$newlm19)
