#install.packages("locClassData", repos="http://R-Forge.R-project.org")
#install.packages("locClass", repos="http://R-Forge.R-project.org")
library(party) #mob
library(mob)
library(stats) 
library(locClass) # Multinomial
library(locClassData)
library(flexmix)
library(nnet)
# Data upload
load("fundamental1.RData")
# Split the set into test and training
smp_size <- floor(0.8 * nrow(fundamental))
set.seed(123)
train_ind <- sample(seq_len(nrow(fundamental)), size = smp_size)
train <- fundamental[train_ind, ]
test <- fundamental[-train_ind, ]
#### Multinomial regression with mob ######
Resultsm3<-function(x){
  MSE<-mean(residuals(x)^2)
  Deviance<-deviance(x)
  LogLik<-logLik(x)
  AIC<-AIC(x)
  pred_class <- predict(x, newdata = test, out = "class")
  Accuracy<-1-mean(pred_class!=test$Perf_2017_ord3) 
  Indicators<-(data.frame(MSE ,Deviance ,LogLik,AIC, Accuracy))
}

      ###Multinomial analysis with 3 categories in the response variable ###
## Multinomial without mob ####
funtodas<-multinom(Perf_2017_ord3 ~ Country + Indus + 
                     St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                     Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                     BudgetB + PublicD + Unemploy + CurrentB + TradeB, data = train)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB),lower=~+ TradeB)
multinom3_withoutmob<-multinom(Perf_2017_ord3~Country + St + AsstT + Pmargin + 
                             ROE + SolvR + FinancPL + Gearing + TobinsQ + GDPr + Inflat + 
                             BudgetB + PublicD + Unemploy, data = train, iter = 100)
# Multinomial with mob: Step regression variables #####
ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 80, trim = 0.1,
                    breakties = FALSE, parm = NULL, verbose = TRUE, objfun = deviance)
system.time(MOB_multi3 <- mob(Perf_2017_ord3 ~Country + St + ROE + TobinsQ + 
                                GDPr + BudgetB + PublicD + CurrentB|  Country  + 
                                St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                                Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                                BudgetB + PublicD + Unemploy + CurrentB + TradeB
                              ,control = ctrl, data = train, trace = FALSE,
                              model = multinomModel))
# Multinomial with mob: best model ####
MOB_multi3_best <- mob(Perf_2017_ord3 ~  Country + St + AsstT + Pmargin| Country + St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                                      Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                                      BudgetB + PublicD + Unemploy + CurrentB + TradeB
                                    ,control = ctrl, data = train, trace = FALSE,
                                    model = multinomModel)
# For this model, the categorical variables have been deleted as variables for the regression model,
# To improve the display of the model and its interpretation,
# The indus variable has also been deleted  as a partitioning variable, due to possible derived errors.
rbind(Resultsm3(multinom3_withoutmob),Resultsm3(MOB_multi3),Resultsm3(MOB_multi3_best))

#the best model is the one that includes all the variables, eliminating those that are not
#significant in some leaf nodes, or some redundant variables, such as Country, since it is
#a variable that was formed by the information provided by variables such as GDPr or BudgetB in
#step 3 of the exploratory section.
plot(MOB_multi3_best)
#Most observations in Category 2 are at node 3, while those incategory 1 are grouped at node 4, 
#and at node 6 and node 7 they are somewhat more balancedbetween category 1 and 2. 
#We also see how partition variables are some of the we obtained through the Random forest analysis.
pred_class <- predict(MOB_multi3_best, newdata = test, out = "class")
pred_class_without <- predict(multinom3_withoutmob, newdata = test, out = "class")
mat_conf<-xtabs(~test$Perf_2017_ord3+pred_class)
mat_conf
mat_conf_without<-xtabs(~test$Perf_2017_ord3+pred_class_without)
mat_conf_without
#Although there are no major differences, we do see that the percentage of success is higher without perform the mob. 
#This may be because the multinomial model without mob has selectedthe variables by Stepwise,
#while for the model with mob, this is not possible as it would beneed to be done for each node separately

###Multinomial analysis with 5 categories in the response variable ###
Resultsm5<-function(x){
  MSE<-mean(residuals(x)^2)
  Deviance<-deviance(x)
  LogLik<-logLik(x)
  AIC<-AIC(x)
  pred_class <- predict(x, newdata = test, out = "class")
  Accuracy<-1-mean(pred_class!=test$Perf_2017_ord5) 
  mat_conf<-xtabs(~test$Perf_2017_ord5+pred_class)
  Indicators<-(data.frame(MSE ,Deviance ,LogLik,AIC,Accuracy))
}
## Multinomial without mob ####
funtodas<-multinom(Perf_2017_ord5 ~ Country + Indus + 
                     St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                     Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                     BudgetB + PublicD + Unemploy + CurrentB + TradeB, data = train)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB),lower=~+ TradeB)
multinom5_withoutmob<-multinom(Perf_2017_ord5~Country + St + ROE + SolvR + 
                             Tax + TobinsQ + GDPr + BudgetB + PublicD + CurrentB + AsstT, data = train)
# Multinomial with mob and step ###
MOB_multi5 <- mob(Perf_2017_ord5 ~Country + St + ROE + SolvR + 
                    Tax + TobinsQ + GDPr + BudgetB + PublicD + CurrentB + AsstT|  Country  + 
                    St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                    Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                    BudgetB + PublicD + Unemploy + CurrentB + TradeB
                  ,control = ctrl, data = train, trace = FALSE,
                  model = multinomModel)
# Best multinomial ###
MOB_multi5_best <- mob(Perf_2017_ord5 ~  St + ROE + TobinsQ + 
                          GDPr + BudgetB| Country + St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                          Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                          BudgetB + PublicD + Unemploy + CurrentB + TradeB
                        ,control = ctrl, data = train, trace = FALSE,
                        model = multinomModel)
rbind(Resultsm5(multinom5_withoutmob),Resultsm5(MOB_multi5),Resultsm5(MOB_multi5_best))
#Surprisingly, the model with the lowest residuals is the multinomial model without
#mob, although in other indicators such as Deviance we would choose the multinomial model with allvariables.
pred_class <- predict(MOB_multi5_best, newdata = test, out = "class")
pred_class_without <- predict(multinom5_withoutmob, newdata = test, out = "class")
mat_conf<-xtabs(~test$Perf_2017_ord5+pred_class)
mat_conf
mat_conf_without<-xtabs(~test$Perf_2017_ord3+pred_class_without)
mat_conf_without

#the success rate may seem low, possibly due to excessive segmentation
#in the response variable which is not ideal for these data

### CONCLUSION ###
#In this case, it is not so clear that recursive partitioning combined with multinomials models improve set fit. 
#This is because the response variable does not have aclear group structure,
#which makes it difficult to define categories. It would be interesting to apply multinomial models 
#with recursive partitioning to a data set with a structurein more defined categories.


