

library(effectliter)



####################### complex example test ##################
# 
# set.seed(664488)
# 
# N <- 2000
# d <- expand.grid(x=c("control","treat1","treat2"),k1=c("male","female"), 
#                  kateg2=1:2)
# ind <- sample(1:nrow(d), size=N, replace=T)
# d <- d[ind,]
# d$z1 <- rnorm(N)
# d$z2 <- rnorm(N)
# d$z3 <- rnorm(N)
# d$dv <- rnorm(N)

d <- example01

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



############ Example 01 with latent variable ################## 
# 
# set.seed(6636363)
# 
# N <- 300
# eta1 <- rnorm(N,2,2)
# x <- rbinom(N,1,plogis(eta1,location=2))
# k <- sample(c("first","second","third"), size=N, replace=T)
# eta2 <- 0.2 + 0.7*eta1 + 1*x + 0.5*x*eta1 + rnorm(N,0,0.3)
# 
# CPM11 <- eta1 + rnorm(N,0,0.4)
# CPM21 <- 0.5 + eta1 + rnorm(N,0,0.4)
# CPM12 <- eta2 + rnorm(N,0,0.4)
# CPM22 <- 0.5 + eta2 + rnorm(N,0,0.4)
# 
# d <- data.frame(CPM11, CPM21, CPM12, CPM22, x, k)
# example02lv <- d
# save(example02lv, file="data/example02lv.RData")


mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m)*1
CPM22 ~ c(p,p)*1
'

m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)



############ Example 02 with latent variable and K ################## 

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m,m,m,m,m)*1
CPM22 ~ c(p,p,p,p,p,p)*1
'

m1 <- effectLite(y="eta2", x="x", k="k", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)




######### Test Yves idea with one sided main hypothesis
## with complex example
# 
# m1 <- effectLite(data=d, y="dv", z=c("z1"), 
#                  k=c("k1","kateg2"), x="x", 
#                  control="control")
# model <- m1@lavaansyntax@model
# 
# constr <- '
# adjmean0 > 0
# adjmean0 < adjmean1
# adjmean0 < adjmean2
# '
# 
# # model <- paste(model,constr,sep="\n")
# 
# d <- m1@input@data
# m2 <- sem(model, group="cell", group.label=levels(d$cell), data=d,
#     fixed.x=F, group.w.free = TRUE, mimic="mplus")
# summary(m2)
# 
# InformativeTesting(model, R = 1000L, double.bootstrap = "standard", missing="listwise",
#                    constraints = constr, group="cell", 
#                    group.label=levels(d$cell), data=d,
#                    fixed.x=F, group.w.free = TRUE, mimic="mplus")
#
