

library(EffectLiteRlavaan)



####################### complex example test ##################

set.seed(664488)

N <- 2000
d <- expand.grid(x=c("control","treat1","treat2"),k1=c("male","female"), 
                 kateg2=1:2)
ind <- sample(1:nrow(d), size=N, replace=T)
d <- d[ind,]
d$z1 <- rnorm(N)
d$z2 <- rnorm(N)
d$z3 <- rnorm(N)
d$dv <- rnorm(N)

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", control="control")

## 0 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=NULL, x="x", control="control")

## 0 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=NULL, x="x", control="control")


## 1 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k="k1", x="x", control="control")

## 2 K; 3 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3"), 
                 k=c("k1","kateg2"), x="x", control="control")


######################### Example Daten1411.sav #####################

d <- daten1411
test <- effectLite(y="y", x="x", z=c("Iz1g1","Iz1g2","z2"), control="0", data=d)

## per Hand
daten <- test@input@data
model <- test@lavaansyntax@model

covs <- '
Iz1g1 ~~ Iz1g1 + Iz1g2 + z2
Iz1g2 ~~ z2
'
model <- paste(covs, model, sep="\n")

m1 <- sem(model, group="cell", group.label=levels(daten$cell), 
          data=daten, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
          missing="ml")

test <- effectLite(y="y", x="x", z="z2", k="z1", control="0", data=d)



############ Nonortho  EffectLiteR Lisa Mplus Version ################## 
# 
# library(EffectLiteR)
# data(nonortho)
# 
# test <- effectLite(y="y", k="z", x="x", data=nonortho)
# 
# EffectLiteR(y="y",x="x",k="z", control="0",data=nonortho,title="complextest")



############ Example 01 with latent variable (Klauer dataset) ################## 

require(Hmisc)
d <- spss.get("../test_data_notpublic/Datensatz Klauer 07.sav")

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m)*1
CPM22 ~ c(p,p)*1
'

m1 <- effectLite(y="eta2", x="TRAINING", z=c("eta1"), control="0", 
                 measurement=mmtest, data=d, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)



############ Example 02 with latent variable and K (Klauer dataset) ################## 

require(Hmisc)
d <- spss.get("../test_data_notpublic/Datensatz Klauer 07.sav")
set.seed(3311)
d$k1 <- sample(c("male","female"), size=nrow(d), replace=TRUE)

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m,m,m)*1
CPM22 ~ c(p,p,p,p)*1
'

m1 <- effectLite(y="eta2", x="TRAINING", k="k1", z=c("eta1"), control="0", 
                 measurement=mmtest, data=d, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)



## Example 1 with stochastic group sizes
# require(lavaan)
# 
# d <- spss.get("../test_data_notpublic/Datensatz Klauer 07.sav")
# 
# model <- '
# eta2 =~ 1*CPM12 + 1*CPM22
# eta1 =~ 1*CPM11 + 1*CPM21
# CPM11 + CPM12 ~ 0*1
# CPM21 ~ c(m,m)*1
# CPM22 ~ c(p,p)*1
# eta2 ~ c(a000,a100)*1
# eta2 ~ c(a001,a101)*eta1
# eta1 ~ c(mz001,mz101)*1
# group % c(gw0,gw1)*w
# relfreq0 := exp(gw0)/279
# relfreq1 := exp(gw1)/279
# b000 := a000
# b001 := a001
# b100 := a100
# b101 := a101
# g000 := b000
# g001 := b001
# g100 := b100-b000
# g101 := b101-b001
# Ez1 := mz001*relfreq0 + mz101*relfreq1
# Eg1 := g100*1 + g101*Ez1
# '
# 
# m1 <- sem(model, group="TRAINING", group.label=c("0","1"), 
#           data=d, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
#           missing="ml")
# summary(m1)
# 
# 
# ## Example 2 with fixed group sizes
# 
# 
# model <- '
# eta2 =~ 1*CPM12 + 1*CPM22
# eta1 =~ 1*CPM11 + 1*CPM21
# CPM11 + CPM12 ~ 0*1
# CPM21 ~ c(m,m)*1
# CPM22 ~ c(p,p)*1
# eta2 ~ c(a000,a100)*1
# eta2 ~ c(a001,a101)*eta1
# eta1 ~ c(mz001,mz101)*1
# group % c(gw0,gw1)*w
# relfreq0 := 140/279
# relfreq1 := 139/279
# b000 := a000
# b001 := a001
# b100 := a100
# b101 := a101
# g000 := b000
# g001 := b001
# g100 := b100-b000
# g101 := b101-b001
# Ez1 := mz001*relfreq0 + mz101*relfreq1
# Eg1 := g100*1 + g101*Ez1
# '
# 
# m1 <- sem(model, group="TRAINING", group.label=c("0","1"), 
#           data=d, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
#           missing="ml")
# summary(m1)
# 
# 
# ## double check with car:::deltaMethod()
# library(car)
# 
# ## fixed group sizes
# func <- '(a100-a000) + (a101-a001)*(mz001*0.5017921+mz101*0.4982079)'
# deltaMethod(coef(m1),func,vcov(m1))
# 
# ## lavaan stochastic group sizes version 1 (J indicators)
# func <- '(a100-a000) + (a101-a001)*(mz001*(exp(gw0)/279)+mz101*(exp(gw1)/279))'
# deltaMethod(coef(m1),func,vcov(m1))
# 
# ## lavaan stochastic group sizes version 2 (J-1 indicators)
# func <- '(a100-a000) + (a101-a001)*(mz001*(exp(gw0)/279)+mz101*(1-exp(gw0)/279))'
# deltaMethod(coef(m1),func,vcov(m1))
# 
# ## lavaan stochastic group sizes version 3 (J-1 indicators)
# func <- '(a100-a000) + (a101-a001)*(mz001*(1-exp(gw1)/279)+mz101*(exp(gw1)/279))'
# deltaMethod(coef(m1),func,vcov(m1))
# 
# 
# 
# ## EffectLite group sizes
# pv <- coef(m1)
# vm <- vcov(m1)
# 
# 
# ## oder liegt es daran, dass wir J-1 Indikator Variablen nehmen m?ssen, auch evtl. im lavaan...
# 
# 
# ## talk to Yves => maybe we should indeed use a different paramterization, when the approach with poisson maybe leads to biased standard errors when we use J indicators
# 
# 
# ## example with 3 treatment groups
# 
# set.seed(664488)
# 
# N <- 200
# z1 <- rnorm(N,2,1)
# x <- rbinom(N,2,plogis(z1,2))
# Ix1 <- as.numeric(x==1)
# Ix2 <- as.numeric(x==2)
# dv <- 0.1 + 0.3*z1 + 0.3*Ix1 + 0.7*Ix1*z1 + 0.4*Ix2 + 0.8*Ix2*z1 + rnorm(N,0,0.2)
# 
# 
# d <-  data.frame(dv,x,z1)
# 
# ## 1 Z
# 
# model <- '
# dv ~ c(a000,a100,a200)*1
# dv ~ c(a001,a101,a201)*z1
# z1 ~ c(mz001,mz101,mz201)*1
# group % c(gw0,gw1,gw2)*w
# relfreq1 := exp(gw1)/200
# relfreq2 := exp(gw2)/200
# relfreq0 := 1-relfreq2-relfreq1
# b000 := a000
# b001 := a001
# b100 := a100
# b101 := a101
# b200 := a200
# b201 := a201
# g000 := b000
# g001 := b001
# g100 := b100-b000
# g101 := b101-b001
# g200 := b200-b000
# g201 := b201-b001
# Ez1 := mz001*relfreq0 + mz101*relfreq1 + mz201*relfreq2
# Eg1 := g100*1 + g101*Ez1
# Eg2 := g200*1 + g201*Ez1
# '
# m1 <- sem(model, group="x", group.label=c("0","1","2"), 
#           data=d, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
#           missing="ml")
# summary(m1)
# 
