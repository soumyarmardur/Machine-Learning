## Data Gathering ##
employeeDataSet<-read.csv("C:\\Users\\srm\\Downloads\\WA_Fn-UseC_-HR-Employee-Attrition.csv")

### Visualization ###
View(employeeDataSet)
colSums(is.na(employeeDataSet)) ## no null values ###
dim(employeeDataSet) # 1470 row , 35 col ##
summary(employeeDataSet)

table(employeeDataSet$Attrition) ## no 1233, yes 237 ##

## split data into test and train data (70:30) ##

set.seed(125)
employee = sort(sample(nrow(employeeDataSet),nrow(employeeDataSet)*0.80)) ### sample tech to split###
employeeTrain<-employeeDataSet[employee,]
dim(employeeTrain)

employeeTest<-employeeDataSet[-employee,]
dim(employeeTest)


table(employeeTrain$Attrition)


### build model ##

library(C50)
?C5.0
str(employeeDataSet)
employModel<-C5.0(employeeTrain[-c(2,22)],employeeTrain$Attrition,rules=T, trials = 5)
summary((employModel))

## prediction ###
employprediction<-predict(employModel,employeeTest[-c(2,22)])
employprediction

employeeTest$Attrition

### metrics evaluation ##

library(caret)



evaluation<-confusionMatrix(employprediction,employeeTest$Attrition)
evaluation     #### confusion matrix ###
evaluation$byClass

dim(employprediction)
employprediction<-as.data.frame(employprediction)### predict factor form##as.data.frame to convert data frame##
dim(employprediction)
dim(employeeTest)## employ test will be in dataframe ###

str(employprediction)

predictionfile<-cbind(employprediction,employeeTest)
dim(predictionfile)

?write.csv
write.csv(predictionfile,file="C:\\Users\\srm\\Downloads\\EmployeePredictionFile.csv")

## GMODEL INSTALL ## (alternative for confusion matrix)


library(gmodels)

###pruning ###

library(rpart)
pruning<-rpart(Attrition ~.,employeeTrain)### method2 of model building ######
pruning$cptable
prune<-prune(pruning,cp=0.01801802)### to remove least valued feature (least) ####

pruneprediction<-predict(prune,employeeTest, type="class")
pruneprediction

library(caret)
pruneresult<-confusionMatrix(pruneprediction,employeeTest$Attrition)
pruneresult$byClass

### Naive bayes ###

library(e1071)
library(klaR)

NaiveModel<-NaiveBayes(employeeTrain[-c(2,9,27,22)],employeeTrain$Attrition)
?NaiveBayes
str(employeeTrain)
summary(NaiveModel)
NaiveModel
library(caret)
predictnaive<-predict(NaiveModel,employeeTest)
predictnaive
naiveconfusion<-confusionMatrix(predictnaive$class,employeeTest$Attrition)
naiveconfusion$byClass 


###Randomforest ################# 

library(randomForest)
bestmtry<-tuneRF(employeeTrain[-c(2,22)],employeeTrain$Attrition)
RandomModel<-randomForest(employeeTrain[-c(2,22)],employeeTrain$Attrition,mtry =10,ntree = 10)
RandomModel

Randompredict<-predict(RandomModel,employeeTest)
Randompredict

RandomConfusion<-confusionMatrix(Randompredict,employeeTest$Attrition)
RandomConfusion$byClass

### SVM LINEAR ###

library(e1071)
library(caret)

ctrl <- trainControl(method="repeatedcv",number = 10,repeats = 3)#### Crossvalidation####

#Selecting the Optimal Regularization Parameter(C value/Penalization)
grid <- expand.grid(C= c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

str(trainData)

?train
#Model Bulding
employeeTrain=employeeTrain[-22]
str(employeeTrain)
svmLinear_model <- train(Attrition~.,employeeTrain,method = "svmLinear",tuneGrid = grid,trainControl = ctrl)
svmLinear_model

svmLinear_prediction <- predict(SVM_model,employeeTest)
svmLinear_prediction

Cmetrics <- confusionMatrix(svmLinear_prediction,employeeTest$Attrition)
Cmetrics$byClass

### Radial ###

GridRadial <- expand.grid(sigma = c(0.01,0,015,0.2),C= c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
GridRadial
RadialModel <- train(employeeTrain[-c(2,22)],employeeTrain$Attrition, method ="svmRadial",tuneGrid = grid_radial,trainControl= ctrl)
RadialModel
RadialPredict<-predict(RadialModel,employeeTest)
RadialPredict
Cmetrics <- confusionMatrix(RadialPredict,employeeTest$Attrition)
Cmetrics$byClass
savehistory("~/R algorithms/HISTORY R SVM.Rhistory")





