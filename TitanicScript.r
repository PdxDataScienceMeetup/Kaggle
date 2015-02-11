#the initial version of this script was inspired by the very good tutorial at:
#https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
#the file reading script was copied from there to make the script runnable from anyplace.


readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ),
            colClasses=column.types,
            na.strings=missing.types )
}

Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)

train.raw <- readData(Titanic.path, train.data.file,
                      train.column.types, missing.types)
td <- train.raw

#td <- read.csv("train.csv", header=TRUE, colClasses=train.column.types, na.strings=c("NA", "") )

regexpr(", [^.]*", td$Name)->nameMatches

changeTitle <- function(data, oldTitles, newTitle)
{
  data[data %in% oldTitles] <- newTitle
  return (data)
}
changeOtherTitles <- function(data, oldTitles, newTitle)
{
  data[!(data %in% oldTitles)] <- newTitle
  return (data)
}
substr(td$Name, nameMatches + 2, attr(nameMatches, "match.length") + nameMatches -1)->td$Title
table(td$Title)
td$Title<-changeTitle(td$Title, c("Mlle", "Ms"), "Miss")
td$Title<-changeTitle(td$Title, c("Lady", "the Countess"), "fNoble")
td$Title<-changeTitle(td$Title, c("Mme", "fNoble"), "Mrs")
td$Title<-changeOtherTitles(td$Title, c("Miss", "Master", "Mrs", "Mr"), "mNoble")
table(td$Title)
td$Title<-as.factor(td$Title)

#structural impution
td$Age->age
for(i in unique(td$Title))
{
  age[td$Title==i & is.na(age)] <- median(age[td$Title==i & !is.na(age)])
}
td$Age<-age



#install.packages("pROC")
require(pROC)

cv<-function (f, data, threshold=.5, numberOfFolds=10, numberOfRepetitions=1, callToBuildModel, callToPredict)
{
  #f is a function of the form passed to lm (e.g., y~x)
  #data is a dataframe that has a Survived column
  
  #create empty results vectors
  auc.values<-c()
  accuracy.values<-c()

  for(k in 1:numberOfRepetitions)
  {
    #create a vector that shows which fold each element will be used as test data for
    slices<-cut(seq(1, nrow(data)), breaks=numberOfFolds, labels=FALSE)[sample(nrow(data))]
    
    for(i in 1:numberOfFolds)
    {
      trainData<-data[slices!=i,]
      testData<-data[slices==i,]
      model<-callToBuildModel(f, trainData)
      testData$prob <- callToPredict(model, testData)
      
      #if the predicted probability is 50% or better, then yes, otherwise no.
      prediction<-testData$prob>=threshold
      testData$Prediction<-as.integer(prediction)
      
      #store accuracy and auc for this fold
      auc.values<-c(auc.values, roc(Survived ~ prob, data=testData)$auc)
      accuracy<-sum(testData$Prediction==testData$Survived)/nrow(testData)
      accuracy.values<-c(accuracy.values, accuracy)
      
    }
  }
  return(data.frame(accuracy.values, auc.values))
}

cv.glm<-function(f, data, threshold=.5, numberOfFolds=10, numberOfRepetitions=1)
{ 
  cv(f, data, threshold, numberOfFolds, numberOfRepetitions
     ,  function(f, trainData)
     {
       return(glm(f, trainData, family=binomial("logit")))
     }
     , function(model, testData)
     {
       return (predict(model, newdata = testData, type = "response"))
     }
  )
}

cv.naiveBayes<-function(f, data, threshold=.5, numberOfFolds=10, numberOfRepetitions=1)
{ 
  cv(f, data, threshold, numberOfFolds, numberOfRepetitions
     , function(f, trainData)
     {
       return(naiveBayes(f, trainData))
     }
     , function(model, testData)
     {
       return (predict(model, newdata = testData, type = "raw")[,2])
     }
  )
}

set.seed(Sys.time())
summary(cv.glm(Survived~Title+I(Title=="Master"):I(Parch+SibSp)+Pclass, td, .5, 10, 10))#best
model<-glm(Survived~Title+I(Title=="Master"):I(SibSp+Parch)+Pclass+Embarked, td, family=binomial("logit"))
model
anova(model)


#graphs
library(sm)

plot.new()
sm.density.compare(td$Age, td$Survived)


plot.new()
sm.density.compare(td$Age, td$Title) 
colfill<-c(2:(2+length(unique(td$Title)))) 
legend(locator(1), legend=unique(td$Title), fill=colfill)


plot.new()
par(mfrow=c(2,3))
for(i in unique(td$Title))
{
  t<-td[td$Title==i,]
  mosaicplot(t$SibSp~t$Survived, main=paste(i, sum(td$Title==i)))
}

plot.new()
par(mfrow=c(2,3))
for(i in unique(td$Title))
{
  t<-td[td$Title==i,]
  mosaicplot(t$Parch~t$Survived, main=paste(i, sum(td$Title==i)))
}

plot.new()
par(mfrow=c(2,3))
for(i in unique(td$Title))
{
  t<-td[td$Title==i,]
  mosaicplot(t$Parch+t$SibSp~t$Survived, main=paste(i, sum(td$Title==i)))
}

plot.new()
par(mfrow=c(1,1))
MasterOnly<-td[td$Title=="Master",]
mosaicplot(MasterOnly$Parch+MasterOnly$SibSp~MasterOnly$Survived, main=paste(i, sum(td$Title==i)))
