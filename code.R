library(leaps)
library(nnet)
library(randomForest)
library(class)
library(MASS)
library(stats)
library(gbm)

wine.red <- read.csv("/home/rohitb/Desktop/StatInfProject/winequality-red.csv", header=TRUE, sep=";")
wine.white <- read.csv("/home/rohitb/Desktop/StatInfProject/winequality-white.csv", header=TRUE, sep=";")

wine.red.total <- wine.red
wine.red.total$class <- 0
wine.white.total <- wine.white
wine.white.total$class <- 1

wine.total <- rbind(wine.red, wine.white)
wine.total.classificiation <- rbind(wine.red.total, wine.white.total)

#Linear Regressions
lm.basic.fit.red <- lm(quality ~ . , data=wine.red)
summary(lm.basic.fit.red)
lm.basic.fit.white <- lm(quality ~ . , data=wine.white)
summary(lm.basic.fit.white)
lm.basic.fit <- lm(quality ~ ., data=wine.total)
summary(lm.basic.fit)

#ANOVA
anova(lm.basic.fit.red)
anova(lm.basic.fit.white)
anova(lm.basic.fit)


#Subset Variable Selection
regsubsets.red.out.exhaustive <- regsubsets(quality ~ ., data=wine.red, nbest=1, nvmax=NULL, method="exhaustive")
summary(regsubsets.red.out.exhaustive)
a <- summary(regsubsets.red.out.exhaustive)
a$adjr2

regsubsets.red.out.forward <- regsubsets(quality ~ ., data=wine.red, nbest=1, nvmax=NULL, method="forward")
summary(regsubsets.red.out.forward)
a <- summary(regsubsets.red.out.forward)
a$adjr2


regsubsets.red.out.backward <- regsubsets(quality ~ ., data=wine.red, nbest=1, nvmax=NULL, method="backward")
summary(regsubsets.red.out.backward)
a <- summary(regsubsets.red.out.backward)
a$adjr2


regsubsets.red.out.seqrep <- regsubsets(quality ~ ., data=wine.red, nbest=1, nvmax=NULL, method="seqrep")
summary(regsubsets.red.out.seqrep)
a <- summary(regsubsets.red.out.seqrep)
a$adjr2

modelStr = quality ~ volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol
lm.red.8var <- lm(modelStr, data=wine.red)
summary(lm.red.8var)
par(mfrow = c(1,1))
sresid <- studres(lm.red.8var)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 
ncvTest(lm.red.8var)

anova(lm.red.8var)
par(mfrow = c(1,1))
plot(lm.red.8var)
plot(lm.red.8var, scale("r2"))
plot(lm.red.8var, scale("adjr2"))
plot(lm.red.8var, scale = "Cp")
plot(lm.red.8var, scale = "bic")

plot(train.red$total.sulfur.dioxide, train.red$free.sulfur.dioxide)
plot(train.red)


#Calculating the conditioning number of the correlation matrix
wine.red.noReponse <- wine.red[-12]
wine.red.noresp.maxtrix <- as.matrix(wine.red.noReponse)
transposeWine.red <- t(wine.red.noresp.maxtrix)%*%wine.red.noresp.maxtrix
rcond(transposeWine.red)


wine.red.8var.noResp <- wine.red[-c(1,3,8,12)]
wine.red.8var.noReponse <- wine.red[-12]
wine.red.8var.noresp.matrix <- as.matrix(wine.red.8var.noResp)
transposeWine.8var.red <- t(wine.red.8var.noresp.matrix)%*%wine.red.8var.noresp.matrix
rcond(transposeWine.8var.red)


#after center and scaling the data
red.scale <- scale(wine.red, center = TRUE, scale = TRUE)
red.scale.noResp <- red.scale[,-12]
red.scale.noResp.matrix <- as.matrix(red.scale.noResp)
transpose.red.scale <- t(red.scale.noResp.matrix)%*% red.scale.noResp.matrix
rcond(transpose.red.scale)

regsubsets.red.scale.exhaustive <- regsubsets(quality ~ ., data=red.scale.df, nbest=1, nvmax=NULL, method="exhaustive")
summary(regsubsets.red.scale.exhaustive)
a <- summary(regsubsets.red.scale.exhaustive)
a$adjr2


#multinomial logistic regression
set.seed(5)
wine.red <- read.csv("/home/rohitb/Desktop/StatInfProject/winequality-red.csv", header=TRUE, sep=";")
x <- sample(c(1:1599), 300, replace=FALSE)
train.red <- wine.red[-x,]
test.red <- wine.red[x,]

#train.red$quality <- as.factor(train.red$quality)
#test.red$quality <- as.factor(test.red$quality)
model = as.factor(quality) ~ volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol
multinom.model <- multinom(model, data=train.red)
#z <- summary(multnom.model)$coefficients/summary(multnom.model)$standard.errors
#p <- (1 - pnorm(abs(z), 0, 1)) * 2

#TRAIN MSE
predictVals <- predict(multinom.model, newdata=train.red)
predictVals <- as.character(predictVals)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (predictVals[i] == train.red$quality[i])
  {
    count=count+1
  }
}    
print(c("count",count))
trainMSE = count/length(train.red$quality)
print(c("trainMSE",trainMSE))
#[1] "trainMSE"          "0.614615384615385"

#TEST MSE

predictVals <- predict(multinom.model, newdata=test.red)
predictVals <- as.character(predictVals)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (predictVals[i] == test.red$quality[i])
  {
    count=count+1
  }
}
print(c("count",count,"test.length",length(test.red$quality)))
testMSE = count/length(test.red$quality)
print(c("testMSE",testMSE))
#[1] "testMSE"           "0.595317725752508"

#knn
k=5
x <- sample(c(1:1599), 300, replace=FALSE)
train.red <- wine.red[-x,]
test.red <- wine.red[x,]

predictVals <- knn(train.red, train.red, train.red$quality, k)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (predictVals[i] == train.red$quality[i])
  {
    count=count+1
  }
}    
print(c("count",count,"train.length",length(train.red$quality)))
trainMSE = count/length(train.red$quality)
print(c("trainMSE",trainMSE))
#[1] "trainMSE"          "0.727692307692308"


predictVals <- knn(train.red, test.red, train.red$quality, k)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (predictVals[i] == test.red$quality[i])
  {
    count=count+1
  }
}    
print(c("count",count,"test.length",length(test.red$quality)))
testMSE = count/length(test.red$quality)
print(c("testMSE",testMSE))
plot(predictVals, test.red$quality, xlab="predicted values", ylab="actual values")
#[1] "testMSE"           "0.588628762541806"

#Random Forest
set.seed(5)
wine.red <- read.csv("/home/rohitb/Desktop/StatInfProject/winequality-red.csv", header=TRUE, sep=";")
x <- sample(c(1:1599), 300, replace=FALSE)
train.red <- wine.red[-x,]
test.red <- wine.red[x,]
fit <- randomForest(as.factor(quality) ~ volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data=train.red, importance=TRUE, ntree=3000)
predictVals <- predict(fit, train.red)
predictVals <- as.character(predictVals)
i =0
count <- rep(0,10)
total <- rep(0,10)
for (i in 1:length(predictVals))
{
  total[train.red$quality[i]] = total[train.red$quality[i]]+1
  if (as.integer(predictVals[i]) == as.integer(train.red$quality[i]))
  {
#    print(c(predictVals[i], train.red$quality[i]))
#    print("Yes")
    count[train.red$quality[i]] = count[train.red$quality[i]]+1
  }
}    
for (i in 1:10)
{
  print (c(count[i],total[i]))
  print(c("Quality", i," Value", as.double(count[i])/total[i]))
}
#[1] "trainMSE"          "0.727692307692308"

predictVals <- predict(fit, test.red)
predictVals <- as.character(predictVals)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (predictVals[i] == test.red$quality[i])
  {
    count=count+1
  }
}    
print(c("count",count,"test.length",length(test.red$quality)))
testMSE = count/length(test.red$quality)
print(c("testMSE",testMSE))
#[1] "testMSE"           "0.709030100334448"

predictVals <- predict(fit, test.red)
predictVals <- as.character(predictVals)
count <- rep(0,10)
total <- rep(0,10)
for (i in 1:length(predictVals))
{
  total[test.red$quality[i]] = total[test.red$quality[i]]+1
  if (as.integer(predictVals[i]) == as.integer(test.red$quality[i]))
  {
    #    print(c(predictVals[i], train.red$quality[i]))
    #    print("Yes")
    count[test.red$quality[i]] = count[test.red$quality[i]]+1
  }
}    
for (i in 1:10)
{
  print (c(count[i],total[i]))
  print(c("Quality", i," Value", as.double(count[i])/total[i]))
}
#[1] "testMSE"           "0.709030100334448"

a = c(wine.red$quality,wine.white$quality)
b = rep(1,length(wine.red$quality))
temp = rep(0,length(wine.white$quality))
d = c(b,temp)
data2 = data.frame(a,d)

lm.basic.fit.red <- lm(a ~ d , data2)
summary(lm.basic.fit.red)

#Linear Regressions
lm.basic.fit.red <- lm(quality ~ . , data=wine.red)
x = resid(lm.basic.fit.red)

wilcox.test(x, y = NULL,alternative = "two.sided",mu = 0, paired = FALSE, exact = NULL, correct = TRUE,conf.int = FALSE, conf.level = 0.95)

#Regression And Classification

modelStr = quality ~ volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol
lm.red.8var.model <- lm(modelStr, data=train.red)
summary(lm.red.8var.model)

predictVals <- predict(lm.red.8var.model, test.red)
typeof(predictVals)
i =0
count = 0
for (i in 1:length(predictVals))
{
  if (round(predictVals[i]) == test.red$quality[i])
  {
    count= count+1
  }
}
print(c("count",count,"test.length",length(test.red$quality)))
testF = count/length(test.red$quality)
print(c("testF",testF))

#Gradient Boosted Tree using the decision tree as the learning model.

gbmM <- gbm(as.factor(quality) ~ volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data=train.red, n.trees=2000, interaction.depth=2)
plot.gbm(gbmM)