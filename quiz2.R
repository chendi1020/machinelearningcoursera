library(AppliedPredictiveModeling)

##Q1
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



library(caret)

#Q2
set.seed(1000)
data(concrete)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

head(rownames(training))

library(Hmisc)
training$V1 <- cut2(training$FlyAsh, g=2)
qplot( inTrain, CompressiveStrength,colour=V1,data = training)+geom_smooth(method='lm',formula=y~x)
training$V2 <- cut2(training$Age, g=4)

qplot( inTrain, CompressiveStrength,colour=V2,data = training)+geom_smooth(method='lm',formula=y~x)

cor(training$FlyAsh, training$CompressiveStrength)

qplot( inTrain, CompressiveStrength,data = training) + geom_smooth(method='lm',formula=y~x)



#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


hist(training$Superplasticizer)


#Q4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL <- grep('^IL', names(training) )

selectvar <- training[, IL]

trans <- prcomp(selectvar, center = T, scale. = T)

percentVariance <- trans$sd^2/sum(trans$sd^2)*100

percentVariance

sum(head(percentVariance, 6))
sum(head(percentVariance, 7))


#Q5
selectvar1 <- training[,c(1, IL)]
trans1 <- preProcess(selectvar1[,-1],
                     method = c("BoxCox", "center", "scale", "pca"))

transformed <- predict(trans1, selectvar1)

transformed1 <- transformed[,1:8]

t <- predict(trans1, testing[,c(1,IL)])


#PCA
fit1 <- train(transformed1[,-1], transformed1[,1], method = 'glm')
p<- predict(fit1, t)
table(p, t$diagnosis)
confusionMatrix(p, t$diagnosis)

(3+56)/82

#non pCA
fit2 <- train(selectvar1[,-1], selectvar1[,1], method = 'glm')
table(predict(fit2,testing), testing$diagnosis)
confusionMatrix(predict(fit2,testing), testing$diagnosis)
(2+51)/82