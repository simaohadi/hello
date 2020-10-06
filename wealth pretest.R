## source of wealth analysis

library(readxl)
library("dplyr")
library(data.table)
library(Hmisc)
library(skimr)
library(pastecs)
library("ggpubr")
library(purrr)
library(tidyr)
library(fmsb)
library(car)
library(AER)
setwd("~/Desktop/Odonatech/Survey/survey pretest/Analysis")




data <- read_excel("data cleaned.xlsx", sheet = 3, col_names = TRUE)
dataC <- read_excel("data cleaned.xlsx", sheet = 4, col_names = TRUE)
dataCountry <- read_excel("data cleaned.xlsx", sheet = 5, col_names = TRUE)
dataEU <- read_excel("data cleaned.xlsx", sheet = 6, col_names = TRUE)

data<- data.table(dataC)


#summary statistics----
n<- 10
dim(data)  
names(data)  
head(data, n)  
summary(data[,n])  
sapply(data, class)  

## descriptive
res <- stat.desc(data[, c("Ybirth" , "School", "Gender1", "NoHousehold", "Income",
                          "salary", "Spouse", "Div", "Rent", "Retirement", "LifeInsu", "Inherit", "Lottery", "Others",
                          "CurrentAsset", "PhysicalAsset", "Debt","PresentBias", "TimeDis", "Gneezy", "FinLit",
                           "OverConf1", "OverConf2", "LossAv", "RiskAv", "ProbDis", "Anchor")])

round(res, 2)


res <- stat.desc(data[, c("Ybirth" , "School", "Gender1", "NoHousehold", "Income",
                          "salary", "Spouse", "Div", "Rent", "Retirement", "LifeInsu", "Inherit", "Lottery", "Others",
                          "CurrentAsset", "PhysicalAsset", "Debt")])

View(round(res, 2))

impdata<- data[, c("Ybirth" , "School", "Gender1", "NoHousehold", "Income",
                   "salary", "Spouse", "Div", "Rent", "Retirement", "LifeInsu", "Inherit", "Lottery", "Others",
                   "CurrentAsset", "PhysicalAsset", "Debt","PresentBias", "TimeDis", "Gneezy", "FinLit",
                   "OverConf1", "OverConf2", "LossAv", "RiskAv", "ProbDis", "Anchor")]
        
        
impdata %>%
        keep(is.numeric) %>% 
        gather() %>% 
        ggplot(aes(value)) +
        facet_wrap(~ key, scales = "free") +
        geom_histogram()


#Correlation

library("Hmisc")
res2 <- rcorr(as.matrix(impdata))
res2

flattenCorrMatrix <- function(cormat, pmat) {
        ut <- upper.tri(cormat)
        data.frame(
                row = rownames(cormat)[row(cormat)[ut]],
                column = rownames(cormat)[col(cormat)[ut]],
                cor  =(cormat)[ut],
                p = pmat[ut]
        )
}

tab <- flattenCorrMatrix(res2$r, res2$P)



# mutation of new varibales----

dataC <- dataC%>%
        mutate(outsideD = OtherThanSal>0)

dataC <- dataC %>%
        mutate(Nsource = rowSums(dataC[,c("salary", "Spouse", "Div", "Rent", "Retirement", "LifeInsu", "Inherit", "Lottery", "Others")]!=0))

dataC <- dataC%>%
        mutate(highInc = Income>=4)

dataC <- dataC %>%
        mutate(relativeInc = (Income-mean(Income))/sd(Income))


dataC <-  dataC %>%
        mutate(IncomeDummy = Income>=4)

dataC <-  dataC %>%
        mutate(AgeDummy = Ybirth>=30)


dataC <- dataC %>%
        mutate(Wealth=CurrentAsset+PhysicalAsset)




K<- data.frame(level=c(1:12),amount=c(5,15,25,35,45,55,65,75,85,95,120,150))

for(i in 1:nrow(dataC)){
        dataC$Incomelevel[i] <- K[dataC$Income[i],2] 
}
x 

# Figures ----

sp <- ggscatter(data, x = "Income", y = "RiskAv",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "outside", palette = "jco", # Color by groups "cyl"
                shape = "outside"                   # Change point shape by groups "cyl"
)+
        stat_cor(aes(color = outside), label.x = 3)       # Add correlation coefficient
sp

sp <- ggscatter(data, x = "Income", y = "TimeDis",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "outside", palette = "jco", # Color by groups "cyl"
                shape = "outside"                   # Change point shape by groups "cyl"
)+
        stat_cor(aes(color = outside), label.x = 3)       # Add correlation coefficient
sp

sp <- ggscatter(data, x = "Income", y = "Anchor",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "outside", palette = "jco", # Color by groups "cyl"
                shape = "outside"                   # Change point shape by groups "cyl"
)+
        stat_cor(aes(color = outside), label.x = 3)       # Add correlation coefficient
sp


sp <- ggscatter(data, x = "Income", y = "ProbDis",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "outside", palette = "jco", # Color by groups "cyl"
                shape = "outside"                   # Change point shape by groups "cyl"
)+
        stat_cor(aes(color = outside), label.x = 3)       # Add correlation coefficient
sp


sp <- ggscatter(data, x = "Nsource", y = "RiskAv",
                add = "reg.line",               # Add regression line
                conf.int = TRUE                # Add confidence interval

)+
        stat_cor()       # Add correlation coefficient
sp


sp <- ggscatter(data, x = "Nsource", y = "Gneezy",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "highInc", palette = "jco", # Color by groups "cyl"
                shape = "highInc"                   # Change point shape by groups "cyl"
)+
        stat_cor(aes(color = highInc), label.x = 3)       # Add correlation coefficient
sp



# regression analysis of Income----

fit1<-lm(Gneezy ~ Income + Ybirth + Gender + FinLit + School  , data=dataC)
fit2<-lm(Gneezy ~ Income + outsideD + Ybirth + Gender + FinLit  + School , data=dataC)
fit3<-lm(Gneezy ~ Income * outsideD + Ybirth + Gender + FinLit  + School , data=dataC)
fit4<-lm(Gneezy ~ Income + Nsource + Ybirth + Gender + FinLit  + School , data=dataC)

fit4<-lm(Gneezy ~ Income + Ybirth + Gender + FinLit  + School , data=dataC)
fit5<-lm(Gneezy ~ Income + Nsource * Outside + Ybirth + Gender + FinLit  + School , data=dataC)
fit6<-lm(Gneezy ~ Income + Nsource + Outside + Ybirth + Gender + FinLit  + School , data=dataC)

stargazer(fit1, fit2, fit3, fit4, no.space = TRUE)
stargazer(fit4, fit5, fit6, no.space = TRUE)

fit1<-lm(RiskAv ~ Income + Ybirth + Gender + FinLit + School  , data=dataC)
fit2<-lm(RiskAv ~ Income + outsideD + Ybirth + Gender + FinLit  + School , data=dataC)
fit3<-lm(RiskAv ~ Income * outsideD + Ybirth + Gender + FinLit  + School , data=dataC)
fit4<-lm(RiskAv ~ Income + Nsource + Ybirth + Gender + FinLit  + School , data=dataC)
fit5<-lm(RiskAv ~ Income + Nsource * Outside + Ybirth + Gender + FinLit  + School , data=dataC)
fit6<-lm(RiskAv ~ Income + Nsource + Outside + Ybirth + Gender + FinLit  + School , data=dataC)




#fit1<-lm(RiskAv ~ Wealth + Ybirth + Gender + FinLit + School  , data=dataW)
#fit2<-lm(RiskAv ~ Wealth + outsideD + Ybirth + Gender + FinLit  + School , data=dataW)
#fit3<-lm(RiskAv ~ Wealth * outsideD + Ybirth + Gender + FinLit  + School , data=dataW)
#fit4<-lm(RiskAv ~ Wealth + Nsource + Ybirth + Gender + FinLit  + School , data=dataW)
#fit5<-lm(RiskAv ~ Wealth + Nsource * Outside + Ybirth + Gender + FinLit  + School , data=dataW)
#fit6<-lm(RiskAv ~ Wealth + Nsource + Outside + Ybirth + Gender + FinLit  + School , data=dataW)



fit1<-lm(RiskAv ~ IncomeDummy + Gender + Ybirth  + FinLit + School + Wealth, data=dataC)
fit2<-lm(RiskAv ~ Nsource+ IncomeDummy + Gender + Ybirth  + FinLit + School + Wealth, data=dataC)
fit3<-lm(RiskAv ~ Nsource + IncomeDummy , data=dataC)
stargazer(fit1, fit2, fit3,  no.space = TRUE)

fit1<-lm(RiskAv ~ IncomeDummy +  Ybirth  + FinLit + School + Wealth, data=dataC)
fit2<-lm(RiskAv ~ Nsource+ IncomeDummy + Ybirth  + FinLit + School + Wealth, data=dataC)
fit3<-lm(RiskAv ~ Nsource + IncomeDummy , data=dataC)
stargazer(fit1, fit2, fit3,  no.space = TRUE)



## checking for residuals
summary(fit <- lm(Income ~ Gender  + Ybirth + School + NoChild + `Current Country of Residence` , data=dataCountry))

dataCountry$res <- fit$residuals
plot(dataCountry$Gneezy, dataCountry$res)

summary(lm( RiskAv ~ res + outsideD + Nsource + Gender  + Ybirth + School + FinLit, data=dataCountry))
summary(lm( Gneezy ~ res + outsideD + Nsource + Gender  + Ybirth + School + FinLit, data=dataCountry))
##


# The effect of Income # without inattentive participants

summary(lm(RiskAv ~ outsideD+ Ybirth + Gender + FinLit +School + IncomeDummy , data=dataC))
summary(lm(RiskAv ~ Nsource + Ybirth + Gender + FinLit +School + IncomeDummy , data=dataC))
summary(lm(RiskAv ~ outsideD+ Ybirth + Gender + FinLit +School + IncomeDummy , data=dataC))
summary(lm(RiskAv ~ outsideD+ Ybirth + Gender + FinLit +School + IncomeDummy , data=dataC))
summary(lm(RiskAv ~ Nsource + Ybirth + Gender + FinLit +School + IncomeDummy , data=dataC))

summary(lm(RiskAv ~ outsideD+ Ybirth +  FinLit +School + IncomeDummy*Gender  , data=dataC))
summary(lm(RiskAv ~ Nsource + Ybirth + FinLit +School + IncomeDummy*Gender , data=dataC))





summary(lm(RiskAv ~ Income + Ybirth + Gender + FinLit +School  , data=dataC))
summary(lm(Gneezy ~ Income + Ybirth + Gender + FinLit + School  , data=dataC))
summary(lm(RiskAv ~ Income * outsideD + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(Gneezy ~ Income * outsideD + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(RiskAv ~ Income + Nsource + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(Gneezy ~ Income + Nsource + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(RiskAv ~ Income + Nsource + Outside + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(Gneezy ~ Income + Nsource + outside + Ybirth + Gender + FinLit  + School , data=dataC))


summary(lm(RiskAv ~ Income*Gender + Ybirth + FinLit +School  , data=dataC))
summary(lm(Gneezy ~ Income*Gender + Ybirth + FinLit + School  , data=dataC))

summary(lm(RiskAv ~ Income*Gender + outsideD + Ybirth + FinLit +School  , data=dataC))
summary(lm(Gneezy ~ Income*Gender + outsideD+Ybirth + FinLit + School  , data=dataC))

summary(lm(RiskAv ~ Income*Gender + Outside + Ybirth + FinLit +School  , data=dataC))
summary(lm(Gneezy ~ Income*Gender + Outside +Ybirth + FinLit + School  , data=dataC))

summary(lm(RiskAv ~ Income*Gender + Nsource + Ybirth + FinLit +School  , data=dataC))
summary(lm(Gneezy ~ Income*Gender + Nsource +Ybirth + FinLit + School  , data=dataC))

dataC <- dataC %>%
        mutate(Income2=Income^2)


summary(lm(RiskAv ~ Income + Income2 + outsideD + Ybirth + Gender + FinLit  + School , data=dataC))
summary(lm(Gneezy ~ Income + Income2 + outsideD + Ybirth + Gender + FinLit  + School , data=dataC))



summary(lm(RiskAv ~ Income*outside + Ybirth + Gender + FinLit+ School , data=dataC[Income>4]))
summary(lm(Gneezy ~ Income*outside + Ybirth + Gender + FinLit+ School , data=dataC[Income>4]))


summary(fit <- lm(RiskAv ~ Income + Ybirth + Gender + FinLit + Outside +Nsource + `Current Country of Residence`  , data=dataC))
summary(fit <- lm(Gneezy ~ Income + Ybirth + Gender + FinLit + Outside +Nsource +  `Current Country of Residence`  , data=dataC))


summary(fit <- lm(RiskAv ~ Income + Ybirth + Gender + FinLit + Outside + `Current Country of Residence`  , data=dataC))
summary(fit <- lm(Gneezy ~ Income + Ybirth + Gender + FinLit + Outside + `Current Country of Residence` , data=dataC))

summary(fit <- lm(RiskAv ~ Income + Ybirth + Gender + FinLit + Nsource + `Current Country of Residence`  , data=dataC))
summary(fit <- lm(Gneezy ~ Income + Ybirth + Gender + FinLit + Nsource + `Current Country of Residence` , data=dataC))



### New

dataW <- dataC %>%
        filter(CurrentAsset<11& PhysicalAsset<11)


#DV:RiskAv, Gneezy
#Control: Ybirth, Gender, School, FinLit, Wealth, `Current Country of Residence`

control <- dataW[ , c("Ybirth", "Gender", "School", "FinLit", "Wealth", "Current Country of Residence")]

summary(fit <- lm(RiskAv ~ Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
summary(fit <- lm(Gneezy ~ Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))

summary(fit <- lm(RiskAv ~ Income + outsideD + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
summary(fit <- lm(Gneezy ~ Income + outsideD + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))

summary(fit <- lm(RiskAv ~ Income + Outside + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
##
summary(fit <- lm(Gneezy ~ Income + Outside + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
##

summary(fit <- lm(RiskAv ~ Income + Nsource + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
summary(fit <- lm(Gneezy ~ Income + Nsource + Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))

summary(fit <- lm(RiskAv ~ Income + Outside*Nsource+ Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))
summary(fit <- lm(Gneezy ~ Income + Outside*Nsource+ Ybirth + Gender + School + FinLit + Wealth +`Current Country of Residence`  , data=dataW))


dataUK <- dataC %>%
        filter(`Current Country of Residence`=="United Kingdom")

summary(fit <- lm(RiskAv ~ Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
summary(fit <- lm(Gneezy ~ Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))

summary(fit <- lm(RiskAv ~ Income + outsideD + Ybirth + Gender + School + FinLit + Wealth  , data=dataUK))
summary(fit <- lm(Gneezy ~ Income + outsideD + Ybirth + Gender + School + FinLit + Wealth  , data=dataUK))

summary(fit <- lm(RiskAv ~ Income + Outside + Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
##
summary(fit <- lm(Gneezy ~ Income + Outside + Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
##

summary(fit <- lm(RiskAv ~ Income + Nsource + Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
summary(fit <- lm(Gneezy ~ Income + Nsource + Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
summary(fit <- lm(RiskAv ~ Income + Outside*Nsource+ Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))
summary(fit <- lm(Gneezy ~ Income + Outside*Nsource+ Ybirth + Gender + School + FinLit + Wealth   , data=dataUK))





# regression analysis of assets----

dataRM <- data %>%
        filter(CurrentAsset < 11 & PhysicalAsset < 11)

#summary(lm(RiskAv ~ CurrentAsset + PhysicalAsset, data=data))
#summary(lm(RiskAv ~ CurrentAsset + PhysicalAsset, data=dataRM))

dataRMC <- dataC %>%
        filter(CurrentAsset < 11 & PhysicalAsset < 11)

#summary(lm(RiskAv ~ CurrentAsset + PhysicalAsset, data=dataRMC))
#summary(lm(RiskAv ~ CurrentAsset + PhysicalAsset, data=dataRMC))



summary(lm(LossAv ~ Income + Ybirth + Gender + FinLit   , data=data))
summary(lm(Gneezy ~ Income + Ybirth + Gender + FinLit +   , data=data))

summary(lm(RiskAv ~ Income  , data=data))

summary(lm(RiskAv ~ Income + outside + Ybirth + Gender + FinLit  , data=data))
summary(lm(RiskAv ~ Income * outside + Ybirth + Gender + FinLit  , data=data))


## whether the source of money matters in explaining risk aversion


#summary(lm(RiskAv ~ Income + salary + Spouse + Ybirth + Gender + FinLit + OverConf1  , data=data))
#summary(lm(RiskAv ~ Income + salary + Spouse + Div + Rent + Retirement + LifeInsu + Inherit + Lottery
#           + Ybirth + Gender + FinLit + OverConf1, data=data))

#summary(lm(RiskAv ~ Income + salary + Spouse  , data=data))
#summary(lm(RiskAv ~ Income + salary + Spouse + Div + Rent + Retirement + LifeInsu + Inherit + Lottery , data=data))







# robustness check----
summary(lm(Gneezy ~ Income + salary + Spouse  , data=data))






#including dummy for Income and age
#rich 1 poor 0








# 2SLS -----

dataEU <- dataEU%>%
        mutate(outsideD = OtherThanSal>0)

dataEU <- dataEU %>%
        mutate(Nsource = rowSums(dataEU[,c("salary", "Spouse", "Div", "Rent", "Retirement", "LifeInsu", "Inherit", "Lottery", "Others")]!=0))

dataEU <- dataEU%>%
        mutate(highInc = Income>=4)

dataEU <- dataEU %>%
        mutate(relativeInc = (Income-mean(Income))/sd(Income))



dataEU <-  dataEU %>%
        mutate(IncomeDummy = Income>=4)

dataEU <-  dataEU %>%
        mutate(AgeDummy = Ybirth>=30)

dataEU <- dataEU %>%
        mutate(LogInc=log(Income))

dataEU <- dataEU %>%
        mutate(Wealth=CurrentAsset+PhysicalAsset)

Inc.ols <- lm(Incomelevel ~ Ybirth + Gender + School + NoChild +`Current Country of Residence`  , data=dataEU)
summary(Inc.ols)

IncHat <- fitted(Inc.ols)


fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat  , data=dataEU)
summary(fit1)



fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat + IncomeDummy + Wealth + Nsource, data=dataEU)
summary(fit1)
fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat + outsideD +IncomeDummy , data=dataEU)
summary(fit1)
fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat + outsideD + Nsource +IncomeDummy, data=dataEU)
summary(fit1)
fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat + OtherThanSal + Nsource+IncomeDummy , data=dataEU)
summary(fit1)

fit1<-lm(RiskAv ~  Ybirth + Gender + FinLit + School + IncHat + salary + Spouse + Nsource+IncomeDummy , data=dataEU)
summary(fit1)



fit1<-lm(Gneezy ~  Ybirth + Gender + FinLit + School + IncHat + IncomeDummy , data=dataEU)
summary(fit1)
fit1<-lm(Gneezy ~  Ybirth + Gender + FinLit + School + IncHat + Nsource +IncomeDummy  , data=dataEU)
summary(fit1)
fit1<-lm(Gneezy ~  Ybirth + Gender + FinLit + School + IncHat + outsideD +IncomeDummy , data=dataEU)
summary(fit1)
fit1<-lm(Gneezy ~  Ybirth + Gender + FinLit + School + IncHat + outsideD + Nsource +IncomeDummy, data=dataEU)
summary(fit1)
fit1<-lm(Gneezy ~  Ybirth + Gender + FinLit + School + IncHat + Outside + Nsource+IncomeDummy , data=dataEU)
summary(fit1)
