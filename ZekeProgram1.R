#This script assumes that a dataset is loaded into td.  It was written using the Titanic dataset from Kaggle
#you should be able to use your own dataset fairly easily, so the code to read in a dataset is commented out.
#But, it's there for you should you want an example.


library(pROC)
require(pROC)


#train.column.types <- c('integer',   # PassengerId
#                        'factor',    # Survived 
#                        'factor',    # Pclass
#                        'character', # Name
#                        'factor',    # Sex
#                        'numeric',   # Age
#                        'integer',   # SibSp
#                        'integer',   # Parch
#                        'character', # Ticket
#                        'numeric',   # Fare
#                        'character', # Cabin
#                        'factor'     # Embarked
#)
#trainData <- read.csv("train.csv", header=TRUE, colClasses=train.column.types, na.strings=c("NA", "") )
#trainData -> td


names(td)
cv.glm<-function (f, data, numberOfFolds=10)
{
        #f is a function of the form passed to lm (e.g., y~x)
        #data is a dataframe that has a Survived column
        
        #create a vector that shows which fold each element will be used as test data for
        slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]
        
        #create empty results vectors
        auc.values<-c()
        accuracy.values<-c()
        
        for(i in 1:numberOfFolds)
        {
                trainData<-data[slices!=i,]
                testData<-data[slices==i,]
                logitModel<-glm(f, data=trainData, family=binomial("logit"))
                testData$prob <- predict(logitModel, newdata = testData, type = "response")
                
                #if the predicted probability is 50% or better, then yes, otherwise no.
                prediction<-predict(logitModel, newdata = testData, type = "response")>=.5
                testData$Prediction<-as.integer(prediction)
                
                #store accuracy and auc for this fold
                auc.values<-c(auc.values, roc(Survived ~ prob, data=testData)$auc)
                accuracy<-sum(testData$Prediction==testData$Survived)/nrow(testData)
                accuracy.values<-c(accuracy.values, accuracy)
                
        }
        
        return(data.frame(accuracy.values, auc.values))
}

results<-cv.glm(Survived~Title+Pclass+SibSp, td, 4)
