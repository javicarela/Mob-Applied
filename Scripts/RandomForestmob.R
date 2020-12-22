library(party) #mob
library(mob)
library(stats) 
library(mobForest) # Random Forest
library(parallel)
# Data upload
load("fundamental1.RData")
# Split the set into test and training
smp_size <- floor(0.8 * nrow(fundamental))
set.seed(123)
train_ind <- sample(seq_len(nrow(fundamental)), size = smp_size)
train <- fundamental[train_ind, ]
test <- fundamental[-train_ind, ]
######## Random Forest with mob ################
set.seed(123)
rfout <- mobforest.analysis(as.formula(Perf_2017_cuant ~ Country + St + AsstT + ROE  + 
                                         Gearing + TobinsQ + BudgetB + PublicD ), c("Country","St", "AsstT",
                                                                                    "Pmargin","ROE","SolvR","FinancPL","Tax",
                                                                                    "Rliquid",  "Gearing","TobinsQ","GDPr","Inflat",   
                                                                                    "BudgetB","PublicD","Unemploy","CurrentB", 
                                                                                    "TradeB")
                            ,mobforest_controls = mobforest.control(ntree =300, mtry = 3, replace = TRUE,
                                                                    alpha = 0.05, bonferroni = TRUE, minsplit = 80, verbose = TRUE)
                            ,data = train,processors = 1, model = linearModel, seed = 1234, new_test_data = test)
varimplot(rfout)
set.seed(123)
rfout_bi <- mobforest.analysis(as.formula(Perf_2017_bi ~ Country + Pmargin   + 
                                            BudgetB + PublicD), c("Country","St", "AsstT",
                                                                  "Pmargin","ROE","SolvR","FinancPL","Tax",
                                                                  "Rliquid",  "Gearing","TobinsQ","GDPr","Inflat",   
                                                                  "BudgetB","PublicD","Unemploy","CurrentB", 
                                                                  "TradeB")
                               ,mobforest_controls = mobforest.control(ntree =300, mtry = 3, replace = TRUE,
                                                                       alpha = 0.05, bonferroni = TRUE, minsplit = 80, verbose = TRUE)
                               ,data = train,processors = 1, model = glinearModel, seed = 1234, new_test_data = test,family=binomial())
varimplot(rfout_bi)
### CONCLUSIONS ###

1. #The execution time of these two functions is remarkable. If you want to carry out a simpler test, 
#you can reduce the number of fitted trees (ntree =)


 2. #the variables that most affect the timeto make partitions are those that influence all equally, 
#that is, the variables macroeconomic, which makes sense, since the returns of companies have a
#evident relationship with the economic structure of the country in which they operate.