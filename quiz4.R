
#Q1
library(ElemStatLearn)

data(vowel.train)

data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)

library(caret)
mod1 <- train(y ~., method='rf', data= vowel.train)
mod2 <- train(y ~., method='gbm', data=vowel.train)

pred1 <- predict(mod1, vowel.test)
pred2 <- predict(mod2, vowel.test)
confusionMatrix(pred1, vowel.test$y)
confusionMatrix(pred2, vowel.test$y)

agree <- pred1==pred2

confusionMatrix(pred1[agree], vowel.test[agree, 'y'])

#Q2
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)
m1 <- train(diagnosis~., method='rf', data= training)
m2 <- train(diagnosis~., method='gbm', data= training)
m3 <- train(diagnosis~., method='lda', data= training)

p1 <- predict(m1, testing)
p2 <- predict(m2, testing)
p3 <- predict(m3, testing)

combp <- data.frame(p1,p2,p3, diagnosis=testing$diagnosis)
comm <- train(diagnosis ~., method='rf', data=combp)

predm <- predict(comm, combp)

confusionMatrix(predm, testing$diagnosis)
confusionMatrix(p1, testing$diagnosis)
confusionMatrix(p2, testing$diagnosis)
confusionMatrix(p3, testing$diagnosis)

#Q3
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)
md <- train(CompressiveStrength~., method='lasso', data=training)

library(elasticnet)
plot.enet(md$finalModel, xvar = "penalty", use.color = TRUE)


#Q4
library(lubridate) # For year() function below
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

download.file(URL, destfile = "H:/Coursera/Machine Learning/gaData.csv")

#acs <- read.csv("/data.csv", header = T)

dat = read.csv("H:/Coursera/Machine Learning/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)

m <- bats(tstrain)

fcast <- forecast(m, level = 95, h = dim(testing)[1])


sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
        dim(testing)[1]

#Q5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
library(e1071)

m <- svm(CompressiveStrength~., data = training)
pred <- predict(m, testing)
accuracy(pred, testing$CompressiveStrength)
