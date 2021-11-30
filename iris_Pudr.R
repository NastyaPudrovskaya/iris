install.packages("e1071")
install.packages("ROCR")
install.packages("ggplot2")
install.packages("dplyr")

#библиотеки
library(e1071)
library(ROCR)
library(ggplot2)

#чтение файла
iris <- read.csv("iris.csv")

#график1
for(i in 1:50){
  iris[i,5] = 1
}
for(i in 51:150){
  iris[i,5] = 2
}
iris <- lapply(iris, as.numeric)
x <- data.frame(iris$sepal.length,iris$sepal.width,iris$petal.length,iris$petal.width)
y <- iris$variety

set.seed(1)
plot(x, col = (20 - y))

#график2
dat = data.frame(x = x, y = as.factor(y))
svmfit = svm(y ~., data = dat, kernel = "linear",
             cost = 10, scale = F)

plot(svmfit, dat,x.iris.petal.width ~ x.iris.petal.length,
     slice = list(x.iris.sepal.width = 3, x.iris.sepal.length = 3))

#график3
svmfit$index
summary(svmfit)

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svmfit, dat,x.iris.petal.width ~ x.iris.petal.length,
     slice = list(x.iris.sepal.width = 3, x.iris.sepal.length = 3))
svmfit$index
summary(svmfit)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

saveRDS(bestmod, "bestmod.rds")
bestmod <- readRDS("bestmod.rds")

train = sample(150, 50)
table(true = dat[-train, "y"],
      pred = predict(tune.out$best.model,
                     newdata = dat[-train, ]))

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt = svm(y ~., data = dat[train, ], kernel = "radial",
                 gamma = 2, cost = 0.1, decision.values = M)
fitted = attributes(predict(svmfit.opt, dat[train, ],
                            decision.values = M))$decision.values

par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")


svmfit.flex = svm(y ~., data = dat[train, ], kernel = "radial",
                  gamma = 50, cost = 0.1, decision.values = M)
fitted = attributes(predict(svmfit.flex, dat[train, ],
                            decision.values = M))$decision.values
rocplot(fitted, dat[train, "y"], main = "Training Data", add = M, col = "green")

fitted = attributes(predict(svmfit.opt, dat[-train, ],
                            decision.values = M))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test data")
fitted = attributes(predict(svmfit.flex, dat[-train, ],
                            decision.values = M))$decision.values
rocplot(fitted, dat[-train, "y"], add = M, col = "green")
