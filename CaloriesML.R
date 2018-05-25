source("https://bioconductor.org/biocLite.R")
biocLite("multiClust")
library(AppliedPredictiveModeling)
library(e1071)
library(caret)

setwd("D:/R_ML")
#load datasets
calories <- read.csv("ML/calories.csv", stringsAsFactors = F)
exercise <- read.csv("ML/exercise.csv")
#summarise data
summary(exercise$Weight)
hist(exercise$Heart_Rate)

#merge data
ex_cal <- merge(x = exercise, y = calories, by = "User_ID", all.x = TRUE)


#visualization
#scatter plots
plot(ex_cal)
plot(ex_cal$Weight,ex_cal$Height)

#density plots

#use only when you comment the boxplot code in the plotmldata function
par(mfrow=c(1, 7))  # divide graph area in n columns

plotMLdata <- function(coldata, name) {
  par(mfrow=c(1, 2))
  plot(density(coldata), main=paste("Density Plot: ",name), ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(coldata), 2)))
  polygon(density(coldata), col="red")
  
  boxplot(coldata, main=name, sub=paste("Outlier rows: ", boxplot.stats(coldata)$out))
  
}

#outlier rows from blox plots
boxplot.stats(Body_temp)$out

res <- boxplot.stats(Body_temp)

plotMLdata(Age,"Age")
plotMLdata(Body_temp,"Body_temp")
plotMLdata(Heart_rate,"Heart rate")
plotMLdata(Height,"Height")
plotMLdata(Weight,"Weight")
plotMLdata(Duration,"Duration")

#test for normality
round(e1071::skewness(coldata),2)

#box plots for outliers

#machine learning steps

#make gender categorical
Gender <- factor(x = ex_cal$Gender,levels = c("male","female"))

#feature enginering with duration and heart rate
#square both columns
Duration <- ex_cal$Duration^2
Heart_rate <- ex_cal$Heart_Rate^2

#log calories column, we dont want to predict negative calories.
Ln_Calories <- log(ex_cal$Calories)


#normalise data
#scaling into value of 0 and 1
#columns that have a normal distribution, we use zscore normlization
#others, we MinMAx normalization is used.

Age <- nor.min.max(ex_cal$Age)
Height <- scale(ex_cal$Height)
Weight <- scale(ex_cal$Weight)
Duration <- nor.min.max(Duration)
Heart_rate <- scale(ex_cal$Heart_Rate)
Body_temp <- nor.min.max(ex_cal$Body_Temp)

#nor.min.max(ex_cal$Age)[which(ex_cal$User_ID == "16684358")]
#scale(ex_cal$Heart_Rate^2)[which(ex_cal$User_ID == "16684358")]


#model building
# do not use user id and calories
model_excal <- data.frame(User_ID = ex_cal$User_ID,Gender=ex_cal$Gender,
                          Age, Height, Weight, Duration, Heart_rate, Body_temp, Ln_Calories)

#split data training - 70 and test - 30
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(model_excal), 0.7*nrow(model_excal))  # row indices for training data
trainingData <- model_excal[trainingRowIndex, ]  # model training data
testData  <- model_excal[-trainingRowIndex, ]   # test data


#train model to predict log of calories - linear regression


#score model with test data


#evaluate model


#convert back to natural number - exponential value
