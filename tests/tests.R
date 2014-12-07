

library(EffectLiteR)



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

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

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
# 
# d <- daten1411
# test <- effectLite(y="y", x="x", z=c("Iz1g1","Iz1g2","z2"), control="0", data=d)
# 
# ## per Hand
# daten <- test@input@data
# model <- test@lavaansyntax@model
# 
# m1 <- sem(model, group="cell", group.label=levels(daten$cell), 
#           data=daten, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
#           missing="ml")
# 
# test <- effectLite(y="y", x="x", z="z2", k="z1", control="0", data=d)



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

m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE,
                 se="boot", bootstrap=5L)


############ Example 01a with method factor ################## 

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21
mf =~ 1*CPM11 + 1*CPM12

CPM11 + CPM21 + CPM12 + CPM22 ~ 0*1
'

m1 <- effectLite(y="eta2", x="x", z=c("eta1","mf"), control="0", 
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



######### Example with latent z and manifest y #############


mmtest <- '
eta1 =~ 1*CPM11 + 1*CPM21
CPM11~ 0*1
CPM21 ~ c(m,m)*1
'

m1 <- effectLite(y="CPM22", x="x", z=c("eta1"), control="0", 
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


############## Test with non-standard SE ###################

d <- example01

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="boot", bootstrap=5L, control="control")

# ## 2 K; 1 Z
# m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
#                  se="first.order", control="control")
# 
# ## 2 K; 1 Z
# m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
#                  se="robust", control="control")


## TODO: Why can SE not be computed for first.order and robust?
#
# ## run model without effectLite() and computation of parameters
# testdaten <- m1@input@data
# 
# model <- '
# dv ~ c(a000,a010,a020,a030,a100,a110,a120,a130,a200,a210,a220,a230)*1
# dv ~ c(a001,a011,a021,a031,a101,a111,a121,a131,a201,a211,a221,a231)*z1
# z1 ~ c(mz001,mz011,mz021,mz031,mz101,mz111,mz121,mz131,mz201,mz211,mz221,mz231)*1
# group % c(gw00,gw01,gw02,gw03,gw10,gw11,gw12,gw13,gw20,gw21,gw22,gw23)*w
# '
# 
# m1 <- sem(model, group="cell",group.label=levels(testdaten$cell), 
#           data=testdaten, fixed.x=F, group.w.free = TRUE, mimic="mplus",
#           se="robust")
# summary(m1)


########## Tests with missing values ################

### missing in k

d <- example01
d$k1[2] <- NA

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")


#### missing in z
d <- example01
d$z1[10] <- NA

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")


#### missing in both k and z
d <- example01
d$k1[2] <- NA
d$z1[10] <- NA

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 missing="fiml")


########## Tests with fixed cell size ################
d <- example01

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", fixed.cell=TRUE)

############ Test with constraints (NOT RUN) ################
# 
# lavsyntax <- m1@lavaansyntax@model
# lavsyntax <- paste(lavsyntax,"\n", "g101 == 0")
# testdata <- m1@input@data
# 
# 
# m1star <- sem(lavsyntax, group="cell", data=testdata,
#           fixed.x=FALSE, group.w.free = TRUE, mimic="mplus") 


############ Test with empty cell ################

## should give error message...
# d <- subset(example01, subset= !(x=="treat1" & k1=="male"))
# m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
#                  missing="fiml")


############ Tests with no interaction option ########################
## does not work...
# 
d <- example01

## 1 K; 1 Z
m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="no")

m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="2-way")