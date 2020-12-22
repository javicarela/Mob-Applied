library(rattle)
library(rpart)
library(rpart.plot)
set.seed(110420)
#Carga de datos
load("fundamental1.RData")
# División del conjunto en test y entrenamiento
smp_size <- floor(0.8 * nrow(fundamental))
set.seed(123) # Hacer reproducibles los datos
train_ind <- sample(seq_len(nrow(fundamental)), size = smp_size)
train <- fundamental[train_ind, ]
test <- fundamental[-train_ind, ]

#CART
fundamentaltree_bi<-rpart(Perf_2017_bi ~Country + Pmargin + BudgetB + PublicD, data = train,method="class")

fancyRpartPlot(fundamentaltree_bi, main="Classification Tree for the Performance")
plotcp(fundamentaltree_bi) # We choose the tree with 3 nodes.


predictTREE <- predict(mod.TREE, newdata =train, type="class") #ME devolverá una clase
confusionMatrix( as.factor(predictTREE), train$Perf_2017_bi,positive = "1") 
# 70.98% Accuracy
# Conditional Tree
require(party)
mod.CTREE<-ctree(  Perf_2017_bi ~Country + Pmargin + BudgetB + PublicD ,data=train)
plot(mod.CTREE)
require(dplyr)
predictCTREE <- predict(mod.CTREE, newdata = train, type="response") 
confusionMatrix( as.factor(predictCTREE), train$Perf_2017_bi,positive = "1")
# 70.9% Accuracy
