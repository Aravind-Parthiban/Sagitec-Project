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

#Load Dataset
dtg <- read.csv("GILENYA_COPAXONE_conso_grp.csv")

#Remove reduntant columns like (generic name, drug name,doc_id)
dtg<- dtg[,-c(1,2,4,5,41)]

# dummy variables for factors/characters[Not Necessary for GBM but useful for other models like glmnet]
dtg$brand_name <- as.factor(dtg$brand_name)
dtg_dummy <- dummyVars("~.",data=dtg, fullRank=F)
dtg_dv <- as.data.frame(predict(dtg_dummy,dtg))
print(names(dtg_dv))

#Plot to find the Proportion of Brand Drug
barplot(table(dtg$brand_name))
prop.table(table(dtg$brand_name))

#Splitting Dataset For Train & Test
split = 0.8
set.seed(111)
trainIndex<- createDataPartition(dtg$brand_name, times = 1, p = split , list = F)
dtg_train <- dtg[trainIndex,]
dtg_test <- dtg[-trainIndex,]

#Check the Proportion after splitting 
prop.table(table(dtg_train$brand_name))
prop.table(table(dtg_test$brand_name))
#dataset is split into correct proportionate for both train & test 


#Modeling - gbm (Genralized Boosted Model)
#3 times cross-validation using train control
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(dtg_train[,c(1:5,8:36)], dtg_train$brand_name, 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

#Summary to check most important variables 
summary(objModel)
varImp(objModel)

#Tuning parameters: view trees, shrinkage and interaction depth
print(objModel)


#Model evaluation - type RAW- Does Class Prediction - shows how sure the model about its choice 
predictions <- predict(object=objModel, dtg_test[,c(1:5,8:36)], type='raw')
head(predictions)

#Class Prediction result - RAW type
postResample(pred=predictions, obs=as.factor(dtg_test$brand_name))

#Model Evaluatuion - type Probability -- threshold can be controlled -- 
predictions <- predict(object=objModel, dtg_test[,c(1:5,8:36)], type='prob')
head(predictions)

#Area under Curve
auc <- roc(ifelse(dtg_test$brand_name == "COPAXONE",1,0), predictions[[2]])
print(auc$auc)
#Area under the curve: 0.7872 {AUC ranges between 0.5 and 1, where 0.5 is random and 1 is perfect}

#Plotting Important Variable 
plot(varImp(objModel,scale=F))

#Result & Inferences
#Genralized Boosting Method (Uses Boosted Tree) was used to Predict the Classification of Brand Drug "GILENYA vs COPAXONE" - 
#Total Claim Count, Total Day Supply,Overall Brand drug cost and Provides States had major influnece in Classification of Drugs
#GBM yeilds AUC score of 0.7872  
