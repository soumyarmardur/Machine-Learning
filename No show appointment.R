Noshow<-read.csv("C:\\Users\\srm\\Downloads\\noshowappointments (1)\\KaggleV2-May-2016.csv")

View(Noshow)
colSums(is.na(Noshow))
dim(Noshow)
summary(Noshow)
#table(Noshow$No.show)


##set.seed(125)
showratio = sort(sample(nrow(Noshow),nrow(Noshow)*0.7))
showratioTrain<-Noshow[showratio,]
showratioTrain
showratioTest<-Noshow[-showratio,]
showratioTest

##table(showratioTrain$No.show)


library(C50)
str(Noshow)
NoshowModel<-C5.0(showratioTrain[-c(4,5,14)],showratioTrain$No.show,rules = T)
NoshowModel
summary(NoshowModel)


Noshowprediction<-predict(NoshowModel,showratioTest[-c(4,5,14)])
Noshowprediction

showratioTest$No.show


library(caret)
Evaluation<-confusionMatrix(Noshowprediction,showratioTest$No.show)
class(Noshowprediction)
class(showratioTest$No.show) 
Evaluation
Evaluation$byClass


dim(Noshowprediction)
Noshowpredictionduplicate<-data.frame(Noshowprediction)
dim(Noshowprediction)
dim(showratioTest)

str(Noshowprediction)

Noshowframe<-cbind(Noshowpredictionduplicate,showratioTest)
dim(Noshowframe)

write.csv(Noshowframe, file="C:\\Users\\srm\\Downloads\\NOshowexcel.csv")


library(rpart)
predict<-rpart(No.show~.,showratioTrain)
predict$cptable
prune<-prune(predict,cp=0.0100000)

pruneprediction<-predict(prune,showratioTest, type = "class")
pruneprediction


library(caret)
pruneresult<-confusionMatrix(pruneprediction,showratioTest$No.show)
pruneresult$byClass

#### RANDOM FOREST ###
library(randomForest)

bestmtry <- tuneRF(showratioTrain[-c(4,7)],showratioTrain$No.show)
str(showratioTrain)


?randomForest()


Noshowrandomforest<-randomForest(showratioTrain[-c(4,5,14,7)],showratioTrain$No.show,mtry = bestmtry,ntree = 10)
Noshowrandomforest

