

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

res_list <- list()

d <- example01

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 2 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=c("k1","kateg2"), x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 0 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=NULL, x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 0 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=NULL, x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))

m1 <- effectLite(data=d, y="dv", k=c("k1"), x="x", control="control",
                 interactions="X:Z")

## 1 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k="k1", x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 2 K; 2 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 2 K; 3 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3"), 
                 k=c("k1","kateg2"), x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


## 1 K; 4 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3","k1"), 
                 k="kateg2", x="x", control="control")

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))

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

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=TRUE,
                 missing="fiml", syntax.only=FALSE,
                 se="boot", bootstrap=5L)


############ Kirchmann Example with latent variable ############

kirch <- foreign::read.spss("private/data/kirchmann.sav", to.data.frame=T)

mmtest <- generateMeasurementModel(names=c("eta2", "eta1"),
                                   indicators=list("eta2"=c("CESD_13","CESD_23"),
                                                   "eta1"=c("CESD_11","CESD_21")),
                                   ncells=2,
                                   model=c("parallel","parallel"))

m1 <- effectLite(y="eta2", x="con_pat", z=c("eta1"), control="controls", 
                 measurement=mmtest, data=kirch, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)

m1 <- effectLite(y="CESD_3", x="con_pat", z=c("CESD_1"), k=c("sex"),
                 control="controls", 
                 data=kirch, fixed.cell=FALSE, 
                 propscore=c("GAQA_1","RSQA_1","RSQV_1"),
                 missing="fiml", syntax.only=FALSE)


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


# lavTestWald(m1@results@lavresults,m1@lavaansyntax@hypotheses$hypothesis1)
# lavTestWald(m1@results@lavresults,m1@lavaansyntax@hypotheses$hypothesis2)
# lavTestWald(m1@results@lavresults,m1@lavaansyntax@hypotheses$hypothesis3)
# lavTestWald(m1@results@lavresults,m1@lavaansyntax@hypotheses$hypothesis4)


res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


######### Example with latent z and manifest y #############


mmtest <- '
eta1 =~ 1*CPM11 + 1*CPM21
CPM11~ 0*1
CPM21 ~ c(m,m)*1
'

m1 <- effectLite(y="CPM22", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))



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
                 se="boot", bootstrap=5L, control="control", fixed.cell=TRUE)

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="first.order", control="control", fixed.cell=TRUE)

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.sem", control="control", fixed.cell=TRUE)

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.huber.white", control="control", fixed.cell=TRUE)


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

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))


########## Tests with fixed cell size ################
d <- example01

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", fixed.cell=TRUE)

res_list <- c(res_list, rbind(m1@results@Egx,
                              m1@results@Egxgx,
                              m1@results@Egxgk,
                              m1@results@Egxgxk))



############ Test with empty cell ################

## should give error message...
# d <- subset(example01, subset= !(x=="treat1" & k1=="male"))
# m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
#                  missing="fiml")


############ Tests with interaction option ########################
# 
d <- example01

## 1 K; 1 Z
m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="none")

m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="2-way")

m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:K")

m1 <- effectLite(fixed.cell = TRUE, data=d, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:Z")

############ Tests with propensity score ########################
# 
d <- example01

## variance of propscore too low in this example -- causes error in lavaan
## write informative error message
# m1 <- effectLite(y="dv", z=c("z1"), x="x", 
#                  propscore=c("z2"), control="control",data=d,
#                  syntax.only=FALSE)

d <- nonortho
m1 <- effectLite(y="y", x="x", propscore=c("z"), control="0",data=d)

d$z[15] <- NA
m1 <- effectLite(y="y", x="x", propscore=c("z"), control="0",data=d)

m1 <- effectLite(y="y", x="x", propscore=x~z, control="0", data=d)


########## complex survey design #################
# 
# set.seed(2356)
# 
# N <- 800
# Nc <- 40
# csize <- N/Nc
# cid <- rep(1:Nc, each=csize)
# 
# ## treatment
# x <- rep(c(0,1), each=Nc/2)[cid]
# 
# ## covariate
# ujz <- rnorm(Nc, 0, sqrt(0.1))[cid]
# rij <- rnorm(N, 0, sqrt(1.5))
# z <- 0.4 + 0.7*x + ujz + rij
# xz <- x*z ## interaction
# 
# ## outcome
# ujy <- rnorm(Nc, 0, sqrt(0.2))[cid]
# eij <- rnorm(N, 0, sqrt(2))
# y <- 0 + 1*x + 0.6*z + 0.4*xz + ujy + eij
# weights <- plogis(rnorm(N))
# 
# example_multilevel <- data.frame(y, z, x, xz, cid, weights)
# 
# save(example_multilevel, file="example_multilevel.RData")


model <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
                    syntax.only=F, data=example_multilevel, 
                    ids=~cid, weights=~weights)


################ homoscedastic residual variances ##############
d <- example01
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", homoscedasticity=TRUE)



############ Bettinas Example ############


d <- foreign::read.spss("private/data/Gesamtdatei_Klasse 2 mit Erstsprache.sav", to.data.frame=T)
m1 <- effectLite(data=d, y="b_ELFE_Text", x="Gruppe", 
                 control="KG", propscore=c("Rolle","a_ELFE_Text","a_ELFE_Wort",
                                           "a_ELFE_Speed", "a_ELFE_Satz"))

m1 <- effectLite(y="b_ELFE_Text", x="Gruppe", k="Rolle", z="a_ELFE_Text",
                 data=d, control="KG")


########## Example with add command ###########

d <- example01

## test with additional conditional effects
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", add="newpar := g200 + g100 \n newpar2 := g100")

# cat(m1@lavaansyntax@model)

## test with equality constraints
m1 <- effectLite(data=d, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 == 0 \n g201 == 0 ",
                 syntax.only=F)

## test with inequality constraint

m1 <- effectLite(data=d, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 > 0",
                 syntax.only=F)


############# check with results from previous lavaan version ###########

# res_list_save <- res_list
# save(res_list_save, file="private/res_list_save.RData")

load("private/res_list_save.RData")

stopifnot(all.equal(res_list,res_list_save))

for(i in 1:70){
  print(paste0("entry ", i, ": ", all.equal(res_list[i],res_list_save[i])))
}

