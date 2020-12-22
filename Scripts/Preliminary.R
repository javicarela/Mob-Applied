## IMPORT THE LIBRARIES ##
library(magrittr)
library(rio)
library(ggplot2)
library(caret) # Correlation analysis and find variances near to zero
library(lattice)
library(tidyverse)
library(Amelia)
library(dplyr)
library(corrplot)
library(nycflights13)
library(factoextra)
library(ggthemes)
library(grid)
library(mice) #Missing values
library(gridExtra)
  
#convert("fundamental_rto.xlsx", "fundamental_rto.rda")
#convert("countries.xlsx", "countries.rda")
countries<-import("countries.rda")
countries<-countries[,-c(1)]
fundamental_NA<-import("fundamental_rto.rda")
colnames(fundamental_NA)
countries<-data.frame(countries, row.names =countries$Country)
                                     ### Exploratory Analysis ###
## Description of the type of variables ##
glimpse(fundamental_NA)
fundamental_NA[4:30]<-as.numeric(unlist(fundamental_NA[4:30]))
fundamental_NA[4:30]<-round(fundamental_NA[4:30],3)

#

## Exploratory analysis of missing data ##
missmap(fundamental_NA, rank.order = FALSE,main = "Set missing values")


#Eexploration of the response variable with outliers
# if you want to show the graph in a presentation, the plotly library is an interesting option
# PLot  Perf_2017 continious
p10<-ggplot(fundamental_NA, aes(y = Perf_2017)) +
  geom_boxplot(fill = "blue")+ scale_y_continuous(name = "Performance")+
  ggtitle(" Distribution of the variable Perf_2017")+theme_economist() +
 theme(plot.title = element_text(family="Times new roman", hjust = 0.5))
p10

#Binary response variable, for the Logit's models
#Varible binaria
fill <-"blue"
L = c(-15,0,10)
G = c("0" , "1")
Perf_2017_bi <-cut(fundamental_NA$Perf_2017, breaks =L, labels = G , rigth = FALSE,include.lowest = TRUE)
p10<-ggplot(data = fundamental_NA, aes(x=Perf_2017_bi, fill =as.factor(Perf_2017_bi) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Perf_2017_bi ") +
  labs(x = "Perf_2017_bi",y = "Company")+scale_fill_brewer(palette="Blues")  
p10

# Visualización Indus
p13<-ggplot(data = fundamental_NA, aes(x=Indus, fill =as.factor(Indus) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Indus ") +
  labs(x = "Indus",y = "n")  + guides(fill=FALSE)
p13
# Visualización variable Country sin agrupar
p14<-ggplot(data = fundamental_NA, aes(x=Country, fill =as.factor(Country) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Country ") +
  labs(x = "Country",y = "n")  + guides(fill=FALSE)
p14

### MODIFICATION OF VARIABLES (ClusterCountry.R) ### 
countries.sc<-scale(countries)
library(mclust)
mc.res <- Mclust(countries.sc)
fviz_mclust(mc.res, "BIC", palette = "jco") 
print(mc.res$classification)
fundamental_NA <- fundamental_NA %>% mutate(Country = replace(Country, Country == "GB"|Country == "FR"|Country == "IT"
                                                        |Country == "BE"|Country == "ES"|Country == "PT", "1"))
fundamental_NA <- fundamental_NA %>% mutate(Country = replace(Country, Country == "DE"|Country == "NL"|Country == "CH"
                                                        |Country == "NO"|Country == "DK"|Country == "LU", "2"))
fundamental_NA <- fundamental_NA %>% mutate(Country = replace(Country, Country == "IE"|Country == "GR"|Country == "TR"
                                                        |Country == "RO"|Country == "RS"| Country == "CY"|Country == "HR"
                                                        |Country == "MT"|Country == "UA"|Country == "BA", "3"))
fundamental_NA <- fundamental_NA %>% mutate(Country = replace(Country, Country == "RU"|Country == "SE"|Country == "FI"
                                                        |Country == "AT"|Country == "CZ"|Country == "PL"|Country == "HU"
                                                        |Country == "SI"|Country == "IS"|Country == "LT"|Country == "EE"
                                                        |Country == "BG"|Country == "SK"|Country == "LV" ,"4"))
fundamental_NA$Country<-as.factor(fundamental_NA$Country)
p15<-ggplot(data = fundamental_NA, aes(x=Country, fill =as.factor(Country) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Country ") +
  labs(x = "Country",y = "Company")+scale_fill_brewer(palette="Blues")  
p15
### EXPLORATION ###


# St
ggplot(fundamental_NA, aes(y = St)) +
  geom_boxplot(fill = fill)+ scale_y_continuous(name = "Desviación Típica")+
  ggtitle(" Distribution of the variable St")+theme_economist() +
  theme(plot.title = element_text(family="Times new roman", hjust = 0.5))

#AsstT, OperR, Cashf, PL
p1 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = AsstT)) +
  ggtitle(" Distribution of the variable AsstT")+ theme_economist()+ geom_point(colour = "blue")
p2<- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = OperR)) +
  ggtitle(" Distribution of the variable OperR")+ theme_economist()+ geom_point(colour = "blue")
p3 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Cashf)) +
  ggtitle(" Distribution of the variable Cashf")+ theme_economist()+ geom_point(colour = "blue")
p4 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = PL)) +
  ggtitle(" Distribution of the variable PL")+ theme_economist()+ geom_point(colour = "blue")
grid.arrange(p1, p2, p3,p4, nrow = 2, ncol= 2)

summary(fundamental_NA[7:10])

colnames(fundamental_NA)
p5 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = EBITDA)) +
  ggtitle(" Distribution of the variable EBITDA")+ theme_economist()+ geom_point(colour = "blue")
p6<- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = CurrentR)) +
  ggtitle(" Distribution of the variable CurrentR")+ theme_economist()+ geom_point(colour = "blue")
p7 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Pmargin)) +
  ggtitle(" Distribution of the variable Pmargin")+ theme_economist()+ geom_point(colour = "blue")
p8 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = ROE)) +
  ggtitle(" Distribution of the variable ROE")+ theme_economist()+ geom_point(colour = "blue")
grid.arrange(p5, p6, p7,p8, nrow = 2, ncol= 2)
summary(fundamental_NA[11:14])

p9 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = SolvR)) +
  ggtitle(" Distribution of the variable SolvR")+ theme_economist()+ geom_point(colour = "blue")
p10<- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = FinancPL)) +
  ggtitle(" Distribution of the variable FinancPL")+ theme_economist()+ geom_point(colour = "blue")
p11 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Tax)) +
  ggtitle(" Distribution of the variable Tax")+ theme_economist()+ geom_point(colour = "blue")
p12 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = ROA)) +
  ggtitle(" Distribution of the variable ROA")+ theme_economist()+ geom_point(colour = "blue")
grid.arrange(p9, p10, p11,p12, nrow = 2, ncol= 2)
summary(fundamental_NA[15:18])

p13 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Rliquid)) +
  ggtitle(" Distribution of the variable Rliquid")+ theme_economist()+ geom_point(colour = "blue")
p14<- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Rsolv)) +
  ggtitle(" Distribution of the variable Rsolv")+ theme_economist()+ geom_point(colour = "blue")
p15 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = Gearing)) +
  ggtitle(" Distribution of the variable Gearing")+ theme_economist()+ geom_point(colour = "blue")
p16 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = CEOperatR)) +
  ggtitle(" Distribution of the variable CE/OperR")+ theme_economist()+ geom_point(colour = "blue")
grid.arrange(p13, p14, p15,p16, nrow = 2, ncol= 2)
summary(fundamental_NA[19:22])


p17 <- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = TobinsQ)) +
  ggtitle(" Distribution of the variable TobinsQ")+ theme_economist()+ geom_point(colour = "blue")
p18<- ggplot(data = fundamental_NA, mapping = aes(x = Company, y = EaringY)) +
  ggtitle(" Distribution of the variable EaringY")+ theme_economist()+ geom_point(colour = "blue")
grid.arrange(p17, p18, nrow = 1, ncol= 2)
summary(fundamental_NA[23:24])

indice<-1:length(countries$CtoPIB)

p101 <- ggplot(data = countries, mapping = aes(x = indice, y = CtoPIB)) +
  geom_point(colour = "blue") + ggtitle(" Distribution of the variable CtoPIB")+ theme_economist() 
p102<- ggplot(data = countries, mapping =aes(x = indice, y = Inflac)) +
  geom_point(colour = "blue")+ggtitle(" Distribution of the variable Inflac")+ theme_economist()
p103 <- ggplot(data = countries, mapping =aes(x = indice, y = Bpresup)) + 
  geom_point(colour = "blue")+ggtitle(" Distribution of the variable Bpresup")+theme_economist()
p104 <- ggplot(data = countries, mapping =aes(x = indice, y = DeudaP)) +
  geom_point(colour = "blue")+ggtitle(" Distribution of the variable CE/DeudaP")+ theme_economist()

grid.arrange(p101, p102, p103,p104, nrow = 2, ncol= 2)

p105 <- ggplot(data = countries, mapping = aes(x = indice, y = Desempleo)) +
  geom_point(colour = "blue") + ggtitle(" Distribution of the variable Desempleo")+ theme_economist()
p106<- ggplot(data = countries, mapping =aes(x = indice, y = SCC )) +
  geom_point(colour = "blue")+ggtitle(" Distribution of the variable SCC")+ theme_economist()
p107<- ggplot(data = countries, mapping =aes(x = indice, y = Bcom)) +
  geom_point(colour = "blue")+ggtitle(" Distribution of the variable Bcom")+ theme_economist()
grid.arrange(p105,p106,p107, nrow = 1, ncol= 3)
summary(fundamental_NA[24:26])
# Delete
percentage<-apply(is.na(fundamental_NA), 2,mean)
plot(percentage)
abline(h =0.1)
delete_colums<-which(percentage>0.1)
delete_colums
fundamental_NA<-fundamental_NA[,-delete_colums]
#library(mice)
add<-fundamental_NA[c(1:4)]
columns <- c("St" , "AsstT" , "OperR" , "PL" , "CurrentR" , "Pmargin", 
            "ROE" , "SolvR" , "FinancPL" , "Tax" , "ROA" , "Rliquid" , "Gearing" , 
            "TobinsQ" , "GDPr" , "Inflat","BudgetB" , "PublicD" , "Unemploy" , 
            "CurrentB" , "TradeB")
imputed_data <- mice(fundamental_NA[,names(fundamental_NA) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print = F)
complete.data <- mice::complete(imputed_data)
fundamental<-data.frame(add,complete.data)
### Ouliers -->Winsorized means ###
# For variables with positive values ( some ratios, for example)
replace_outliers <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(0, .9), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}
# For variables negative and positive values, like the response variable
replace_outliers_1 <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.2, 0.8), na.rm = removeNA)
  caps <- quantile(x, probs = c(0.1, 0.90), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}
# it is necessary to observe the distribution of each variable, and apply a different function in each case
fundamental$Perf_2017 <- replace_outliers_1(fundamental$Perf_2017)
fundamental$St <- replace_outliers(fundamental$St)
fundamental$AsstT <- replace_outliers(fundamental$AsstT)
fundamental$OperR <- replace_outliers(fundamental$OperR)

fundamental$PL <- replace_outliers_1(fundamental$PL)
fundamental$CurrentR <- replace_outliers(fundamental$CurrentR)
fundamental$Pmargin <- replace_outliers_1(fundamental$Pmargin)
fundamental$ROE <- replace_outliers_1(fundamental$ROE)
fundamental$SolvR <- replace_outliers_1(fundamental$SolvR)
fundamental$FinancPL <- replace_outliers_1(fundamental$FinancPL)
fundamental$Tax <- replace_outliers_1(fundamental$Tax)
fundamental$ROA <- replace_outliers_1(fundamental$ROA)
fundamental$Rliquid <- replace_outliers(fundamental$Rliquid)
fundamental$Gearing <- replace_outliers(fundamental$Gearing)
fundamental$TobinsQ <- replace_outliers(fundamental$TobinsQ)

# cluster of the response variable (for the multinomial models) in 3 and 5 categories
fundamental.PC<-princomp(~. ,data=fundamental[4], cor=TRUE, scores=TRUE)
summary(fundamental)
fundamental.sc<-scale(fundamental[4])
km.res3<-kmeans(fundamental.sc, 3, nstart=100)
Perf_2017_ord3<-km.res3$cluster
km.res5<-kmeans(fundamental.sc, 5, nstart=100)
Perf_2017_ord5<-km.res5$cluster
p11<-ggplot(data = fundamental, aes(x=Perf_2017_ord3, fill =as.factor(Perf_2017_ord3) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Perf_2017_ord3 ") +
  labs(x = "Perf_2017_ord3",y = "Company")+scale_fill_brewer(palette="Blues")  + guides(fill=FALSE)
p11
# Exploration with 5 categories
p12<-ggplot(data = fundamental, aes(x=Perf_2017_ord5, fill =as.factor(Perf_2017_ord5) ))  + geom_bar(colour = "black")+
  ggtitle (" Distribution of the variable Perf_2017_ord5 ") +
  labs(x = "Perf_2017_ord5",y = "Company")+scale_fill_brewer(palette="Blues")  + guides(fill=FALSE)
p12
companies<-fundamental[c(1)]
St<-fundamental$St
Perf_2017_cuant<-fundamental$Perf_2017
qualitative<-fundamental[2:3]
quantitative<-fundamental[6:25]
# Preliminary Analysis
nzv<-nearZeroVar(quantitative, saveMetrics= TRUE)
nzv #We do not have values with variance close to 0
### Correlation analysis ###

varcor<-findCorrelation(cor(quantitative), cutoff=(0.75))
varcor
#If I eliminate those variables that are correlated, we no longer have linear combinations
quantitative<-quantitative[-varcor]
fundamental<-fundamental[,-1]
fundamental<-data.frame(qualitative,Perf_2017_cuant,Perf_2017_bi, St, quantitative,Perf_2017_ord3,Perf_2017_ord5)
fundamental$Perf_2017_bi<- as.factor(fundamental$Perf_2017_bi)
fundamental$Indus<-as.factor(fundamental$Indus)
summary(fundamental)
ls(pattern = "fundamental")
rm(fundamental_NA,fundamental.PC,fundamental.sc,fundamentalprue)
save(fundamental, file = "C:/Users/franc/fundamental1.RData")

