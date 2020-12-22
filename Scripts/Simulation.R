#Simulation
x<-seq(-10,10,0.02)+runif(1001,1,3)
plot(x)
Zeta<-runif(1001,-1,1)
y<-1:1001
a<-1
b<-1
a_1<-0.5
b_1<-2.5
data<-data.frame(x,Zeta,y)
for(i in 1:1001){
  ifelse(data$Zeta[i]>=0,data$y[i] <- a + b*data$x[i]+rnorm(1,0,5), data$y[i]  <- a_1 + b_1*data$x[i]+rnorm(1,0,5))
}
plot(data$x,data$y)
#GLM without mob 
glm1<-glm(y~x, data = data)
res_1<-mean(residuals(glm1)^2)
dev_1<-deviance(glm1)
log_1<-logLik(glm1)
AIC_1<-AIC(glm1)
residuals_glm<-residuals(glm1)
plot(Zeta,residuals_glm) #the algorithm achieves that, with little correlation, it partition the set correctly
#loading Rcommander you can see a better representation of the residuals against the Zeta variable 
#With mob
library(party)
ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 10, trim = 0.1,
                    breakties = FALSE, parm = NULL, verbose = TRUE, objfun = deviance)
mob_s_2<-mob(y~x|Zeta, control = ctrl, data = data, model = linearModel)
plot(mob_s_2)
# We can see how the algorithm has partitioned the set, practically from the Real point of Zeta.
res_2<-mean(residuals(mob_s_2)^2)
dev_2<-deviance(mob_s_2)
log_2<-logLik(mob_s_2)
AIC_2<-AIC(mob_s_2)
MOB<- c("Model without mob","Model with mob")
Residuals<- c(res_1,res_2)
Deviance<-c(dev_1,dev_2)
LogLik <- c(log_1,log_2)
AIC<- c(AIC_1,AIC_2)
Results2 <- data.frame(MOB, Residuals,Deviance, LogLik, AIC)
print(Results2)
