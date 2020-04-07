library(tree)
library(ISLR)
library(dplyr)
library(tidyr)
attach(Carseats)

High = ifelse(Sales <= 8 , "No" , "Yes")

Carseats = data.frame(Carseats , High)

tree.Carseats = tree(High ~ . - Sales , data = Carseats)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats , pretty = 0)

set.seed(2)
train = sample (1 : nrow(Carseats) , 200)
Carseats.test = Carseats[-train , ]
High.test = High[-train]

tree.Carseats = tree (High ~. -Sales , Carseats, subset = train)
plot(tree.Carseats)
text(tree.Carseats , pretty = 0)

tree.pred = predict(tree.Carseats , Carseats.test , type = "class")
table(tree.pred , High.test)
names (tree.Carseats)

##Pruning the tree.
set.seed(3)
cv.carsets = cv.tree(tree.Carseats , FUN = prune.misclass)

par(mfrow=c(1,2))
plot(cv.carsets$size , cv.carsets$dev , type = "b")
plot(cv.carsets$k , cv.carsets$dev , type = "b")

prune.carseats = prune.misclass(tree.Carseats , best = 9)
plot(prune.carseats)
text(prune.carseats , pretty = 0)

tree.pred = predict(prune.carseats , Carseats.test , type = "class")
table (tree.pred , High.test)


prune.carseats = prune.misclass(tree.Carseats , best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats , Carseats.test , type = "class")
table (tree.pred , High.test)


##Fitting regression tree . Boston dataset.

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston) , nrow(Boston)/2)
tree.boston = tree(medv~. , Boston , subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston , pretty = 0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size , cv.boston$dev , type = "b")

prune.boston = prune.tree(tree.boston , best = 5)
plot(prune.boston)
text(prune.boston , pretty = 0)

yhat = predict(prune.boston , newdata = Boston[-train,])
boston.test = Boston[-train , "medv"]
plot(yhat , boston.test)
abline(0,1 , col = 'red')
summary(prune.boston)

## Bagging and Random forest.















