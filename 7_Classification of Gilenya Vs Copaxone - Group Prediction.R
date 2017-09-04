#Packages Used
require(readxl)
require(ggplot2)
require(stringr)
require(data.table)
require(maps)
require(reshape2)
require(corrplot)
require(caret)
require(C50)
require(rpart)
require(RColorBrewer)
require(rattle)
require(rpart.plot)
require(pROC)

#Read Dataset
dtg <- read.csv("GILENYA_COPAXONE_conso_grp.csv")

#Remove reduntant columns like (generic name, drug name,doc_id)
dtg<- dtg[,-c(1,4,5,10)]

#Plot to find the Proportion of Brand Drug Group
barplot(table(dtg$group))
prop.table(table(dtg$group))

#Splitting Dataset For Train & Test
split = 0.8
set.seed(111)
trainIndex<- createDataPartition(dtg$group, times = 1, p = split , list = F)
dtg_train <- dtg[trainIndex,]
dtg_test <- dtg[-trainIndex,]

#Check the Proportion of outcome variable after splitting 
prop.table(table(dtg_train$group))
prop.table(table(dtg_test$group))
#dataset is split into correct proportionate for both train & test 

#Single Tree
oneTree <- C5.0(dtg_train$group~.,data = dtg_train)
oneTree
oneTreePred  <- predict(oneTree, dtg_test)
oneTreeProbs <- predict(oneTree, dtg_test, type ="prob")

#Single Tree Prediction 
postResample(oneTreePred, dtg_test$group)
summary(oneTree)

#Single Ruleset
rules <- C5.0(dtg_train$group ~ ., data = dtg_train, rules = TRUE)

#Single ruleset predictions
postResample(predict(rules, dtg_test), dtg_test$group)
summary(rules)

## Rules can also be boosted
bstTree <- C5.0(dtg_train$group ~ ., data = dtg_train, trials = 10)
bstTree

#Boosted Tree prediction
bstTreePred <- predict(bstTree, dtg_test)
postResample(bstTreePred, dtg_test$group)


#Using control Parameters to impove accuracy (Winnowing)
mod <- C5.0(dtg_train$group ~ ., data = dtg_train,control = C5.0Control(winnow = TRUE,
                                                                             noGlobalPruning = FALSE, 
                                                                             CF = 0.25, 
                                                                             minCases = 2, 
                                                                             fuzzyThreshold = FALSE,  
                                                                             earlyStopping = F))

#Winnowing Prediction 
modPred <- predict(mod, dtg_test)
postResample(modPred, dtg_test$group)

#Using train -- Repeated Cross Validation 
tuned <- train(dtg_train[, c(1:36)], dtg_train$group,
               method = "C5.0", tuneLength = 11,
               trControl = trainControl(method = "repeatedcv", repeats = 10),
               control = C5.0Control(earlyStopping = F),
               metric = "Kappa")

postResample(predict(tuned, dtg_test), dtg_test$group)

#Resampling Profile
plot(tuned, metric = "Kappa")

#RESULTS & INFERENCEs
#Using Decision Tree to build a model to predict the outcomes of drugs classification (GILENYA vs COPAXONE)
#The most important variable in deciding the durg classification is Overall brand Durg cost
#BOOSTING Results {Accuracy  0.6913767, Kappa  0.3425066} Other methodologies yields similar results {Accuracy 0.6807867, Kappa 0.3522708}
#So the model where boosting is done yields better accuracy than other methodologies followed

