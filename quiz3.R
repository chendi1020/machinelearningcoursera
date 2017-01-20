#Q3
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- segmentationOriginal %>% filter(Case=='Train') %>% select(-c(Case,Cell))
testing <- segmentationOriginal %>% filter(Case=='Test')

set.seed(125)
modfit <- train(Class~., method='rpart', data = training )

test1 <- testing %>% filter(TotalIntenCh2 == 23000 & FiberWidthCh1==10 & PerimStatusCh1==2)

summary(modfit$finalModel)
plot(modfit$finalModel, uniform = T)
text(modfit$finalModel, use.n = T, all = T, cex=.8)

library(rattle)
fancyRpartPlot(modfit$finalModel)



library(pgmm)
data(olive)
olive = olive[,-1]
#olive$Area <- as.factor(olive$Area)

newdata = as.data.frame(t(colMeans(olive)))

modfit <- train(Area~., method='rpart', data = olive )

predict(modfit, newdata)



library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
#trainSA$chd <- as.factor(trainSA$chd)

modfit <- train(chd ~ age + alcohol + obesity+ tobacco+typea+ ldl, data=trainSA, method='glm', family='binomial')
predict <- predict(modfit, testSA)
predict1 <- predict(modfit, trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

table(testSA$chd, predict)
table(trainSA$chd, predict1)

missClass(testSA$chd, predict)
missClass(trainSA$chd, predict1)

library(ElemStatLearn)

data(vowel.test)
data(vowel.train)

head(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
library(randomForest)
mod <- randomForest(y ~ ., data = vowel.train)
order(varImp(mod), decreasing = T)
