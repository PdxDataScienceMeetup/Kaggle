# vim:et:sts=2:sw=2

# Kaggle.com Bikeshare Competition benchmarks using a bunch
# of different machine learning methods.
# Ryan McNamara, March 5, 2015

library(randomForest)
library(nnet)
library(class)                         # for knn function
library(chron)

setwd("~/R/Bikeshare")
tr <- read.csv("train.csv")
te <- read.csv("test.csv")

evaluate <- function(dataset, fitness.f, split.p = 0.75) {
  # Determine the accuracy of a model without submitting it
  # to Kaggle. Only uses the training set.
  set.seed(24816)
  n      <- nrow(dataset)
  size   <- n * split.p
  index  <- sample(1:n, size, FALSE)
  new.tr <- dataset[ index,]
  new.te <- dataset[-index,]           # '-' means 'not'
  fit    <- fitness.f(new.te)
  pred   <- predict(fit, new.te)
  # Sum of squared differences to calculate the error
  sqrt(sum((new.te$count - pred)^2)) / nrow(new.te)
}

fmla = paste("count ~ (season + holiday + workingday + weather
                        + temp + atemp + humidity + windspeed)^3")

# Create silly example of a model to benchmark for debugging
# and demonstration purposes.
avg <- function(dataset) {
  ret <- list(avg1 = mean(dataset$count), avg2 = mean(dataset$count))
  class(ret) <- "avg"
  return(ret)
}
predict.avg <- function(x, dataset) rep(x$avg1, nrow(dataset))

my.forest <- function(dataset) {
  randomForest(as.formula(fmla), data=dataset)
}

my.nnet <- function(dataset) {
  nnet(as.formula(fmla), data=dataset, size=10)
}

my.lm  <- function(tr) lm(fmla, data=tr)

my.glm <- function(tr) glm(fmla, data=tr)


myknn <- function(dataset) {
  fit <- list(datetime = dataset$datetime, label = dataset$count)
  class(fit) <- "myknn"
  return(fit)
}

predict.myknn <- function(x, dataset) {
  tr <- as.matrix(as.numeric(as.chron(as.POSIXct(x$datetime)), ncol=1))
  te <- as.matrix(as.numeric(as.chron(as.POSIXct(dataset$datetime)), ncol=1))
  lbl <- as.numeric(x$label)
  myk <- 1
  as.numeric(knn(tr, te, cl=lbl, k=myk))
}


evaluate(tr, avg)                      # Score: 3.45 
evaluate(tr, my.forest)                # Score: 1.99
evaluate(tr, my.nnet)                  # Score: 5.00 (worse than avg?!)
evaluate(tr, my.lm)                    # Score: 2.81
evaluate(tr, my.glm)                   # Score: 2.81
evaluate(tr, myknn)                    # Score: 0.62 (k = 1 works best)
