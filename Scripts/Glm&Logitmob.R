library(party) #mob
library(mob)
library(stats) 
# Carga de datos
load("fundamental1.RData")
# División del conjunto en test y entrenamiento
smp_size <- floor(0.8 * nrow(fundamental))
set.seed(123) # Hacer reproducibles los datos
train_ind <- sample(seq_len(nrow(fundamental)), size = smp_size)
train <- fundamental[train_ind, ]
test <- fundamental[-train_ind, ]

#The first models are trial, since they are quite simplet_1<- proc.time()
ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 80, trim = 0.1,
                    breakties = FALSE, parm = NULL, verbose = TRUE, objfun = deviance) # With logLik we have made too many partitions in some models
Results<-function(x){
  MSE<-mean(residuals(x)^2)
  Deviance<-deviance(x)
  LogLik<-logLik(x)
  AIC<-AIC(x)
  Indicators<-(data.frame(MSE ,Deviance ,LogLik,AIC))
}

options(width = 60)
options(digits = 5)
system.time(MOB1 <- mob(Perf_2017_cuant ~Country + AsstT+Pmargin + St+Rliquid| Country + Indus + St + 
                          AsstT + Pmargin + ROE + SolvR + FinancPL + Tax + Rliquid +Gearing + 
                          TobinsQ +  GDPr +  Inflat +BudgetB + PublicD + Unemploy + CurrentB +TradeB,
                        control = ctrl, data = train, model = linearModel))
t_MOB1<-proc.time()-t_1
plot(MOB1)
Resultss<-rbind(Results(MOB1),Results(glm0))
Time<-c(t_MOB1[3],t_glm0[3])
Resultss<-cbind(Resultss,Time)
print(Resultss)
##########################################

t_1<- proc.time()
funtodas<-glm(Perf_2017_cuant ~ Country + Indus + 
                St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                BudgetB + PublicD + Unemploy + CurrentB + TradeB, data = train)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB),lower=~+ TradeB)
system.time(MOB1 <- mob(Perf_2017_cuant~Country + Indus + St + AsstT + 
                          ROE + SolvR + Rliquid + Gearing + TobinsQ + BudgetB + PublicD + 
                          TradeB |Country + Indus +  St + AsstT + Pmargin + ROE + SolvR +
                          FinancPL + Tax +  Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                          BudgetB + PublicD + Unemploy + CurrentB + TradeB ,
                        control = ctrl, data = train, model = linearModel))
t_MOB1<-proc.time()-t_1

dev.off()
plot(MOB1)
res_part<-mean(residuals(MOB1)^2)
#Brief analysis of the tendency to partition with the variable Indus
comp_indus <-train$Indus
togeth<-data.frame(res_part,comp_indus)
MOB_part<-mob(res_part~1|comp_indus, data = togeth, control = ctrl, model = linearModel)
#We do this to remove the indus variable as a candidate for the partition, as including it can cause 
#the execution time to increase exponentially for some models

## Logit with mob ###
t_2 <- proc.time()
funtodas<-glm(Perf_2017_bi ~ Country + Indus + 
                St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                BudgetB + PublicD + Unemploy + CurrentB + TradeB, data = train, family = binomial)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB),lower=~+ TradeB)

system.time(MOB2 <- mob(Perf_2017_bi ~Country + Indus + ROE + SolvR + 
                          Rliquid + Gearing + TobinsQ + GDPr + BudgetB + PublicD + 
                          CurrentB| Country  + 
                          St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                          Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                          BudgetB + PublicD + Unemploy + CurrentB + TradeB,
                        control = ctrl, data = train, 
                        model = glinearModel, family = binomial()))
t_MOB2<-proc.time()-t_2

# glm todas
t_3 <- proc.time() 
funtodas<-glm(Perf_2017_cuant ~ Country + Indus + 
                St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                BudgetB + PublicD + Unemploy + CurrentB + TradeB, data = train)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB ),lower=~+ TradeB)
glm1<-glm(Perf_2017_cuant~ Country + Indus + St + AsstT + 
            ROE + SolvR + Rliquid + Gearing + TobinsQ + BudgetB + PublicD + 
            TradeB , data = train)

t_glm1<-proc.time()-t_3
#plot(lm.influence(glm1)$hat)

# Logit todas
t_4 <- proc.time() 
funtodas<-glm(Perf_2017_bi ~Country + Indus + 
                St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                BudgetB + PublicD + Unemploy + CurrentB + TradeB , data = train, family = binomial)
step(funtodas , scope = list(upper=~Country + Indus + 
                               St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                               Rliquid +Gearing + TobinsQ +  GDPr +  Inflat +  
                               BudgetB + PublicD + Unemploy + CurrentB + TradeB ),lower=~+ TradeB)
glm2<-glm(Perf_2017_bi~Country + Indus + ROE + SolvR + 
            Rliquid + Gearing + TobinsQ + GDPr + BudgetB + PublicD + 
            CurrentB , data = train, family = "binomial")

t_glm2<-proc.time()-t_4


Resultss<-rbind(Results(MOB1),Results(glm1),Results(MOB2),Results(glm2))
Tiempo <- c(t_MOB1[3],t_glm1[3] ,t_MOB2[3], t_glm2[3])
Resultss <- data.frame(Resultss,Tiempo)
print(Resultss)
#Through the implementation of step () and with respect to first table we have been able to reduce all
#the residuals and the Deviance of the model, but not the LogLik and the AIC that have increased for the
#GLM with mob.


#Now we are going to see how, by eliminating some variables that are not very significant in the terminal nodes, 
#we obtain an improvement in all the quality indicators.
#First in the GLM model:
summary(MOB1)
# We are goint to  Country, AsstT and St for regression, and all others to partitioning, except Indus
# Mejor glm 
t_best <- proc.time() 
system.time(MOB1_best <- mob(Perf_2017_cuant~Country + St + AsstT |Country + 
                          St + AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +
                          Rliquid +Gearing + TobinsQ +  GDPr +  Inflat + BudgetB + 
                          PublicD + Unemploy + CurrentB + TradeB ,control = ctrl,
                        data = train, model = linearModel))
t_MOB1_best<-proc.time()-t_best

#Second in the Logit model:
t_best <- proc.time() 
MOB2_best <- mob(Perf_2017_bi ~ Perf_2017_bi ~ Country + Pmargin + BudgetB + PublicD | Country + St +
                   AsstT + Pmargin + ROE + SolvR + FinancPL + Tax +Rliquid + Gearing +
                   TobinsQ + GDPr + Inflat + BudgetB + PublicD + Unemploy + CurrentB +
                   TradeB,control = ctrl, data = train, 
            model = glinearModel, family = binomial())
t_MOB2_best<-proc.time()-t_best
plot(MOB2_best)
summary(MOB2_best)
round(exp(coef(MOB2_best)),3) 
# We can see the odds ratios. For example, if we are in the node 5, we have more probability to
# have a company with positive performance, Because of the mayor Intercept (Bo)
# Predicctions and Cofunsion Matrix
pred_class_MOB2 <- predict(MOB2_best, newdata = test, out = "class")
cats.pred = rep("0", dim(test)[1])
cats.pred[pred_class_MOB2 > .5] = "1"
mat_conf<-xtabs(~test$Perf_2017_bi+cats.pred)
sum(diag(mat_conf)/length(test$Perf_2017_bi))
# Logit without mob
glm2<-glm(Perf_2017_bi~Country + Pmargin   + 
            BudgetB + PublicD , data = train, family = "binomial")
#Prediction for logit without mob
cats.prob <- predict(glm2, newdata = test, type = "response")
cats.pred = rep("0", dim(test)[1])
cats.pred[cats.prob > .5] = "1"
mat_conf_sin<-xtabs(~test$Perf_2017_bi+cats.pred)
sum(diag(mat_conf_sin)/length(test$Perf_2017_bi))
mat_conf
mat_conf_sin
# To make predictions, the difference in models is not so clear, although the Logit model with
#mob ranks negative returns better, and the logit model without mob ranks somewhat better
#positive returns
#Therefore, the application ofRecursive partitioning continues to be better.
