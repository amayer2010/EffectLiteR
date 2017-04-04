
############## Test with non-standard SE ###################

## bootstrap 2 K; 1 Z
set.seed(02847)

expect_warning({
  m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                   se="boot", bootstrap=5L, control="control", fixed.cell=TRUE)
})

res_boot5_1z2k <- rbind(m1@results@Egx,
                        m1@results@Egxgx,
                        m1@results@Egxgk,
                        m1@results@Egxgxk)

oldres_boot5_1z2k <- read.table("tests/oldres/oldres_boot5_1z2k.dat")
expect_equivalent(res_boot5_1z2k, oldres_boot5_1z2k)


## first order SE 2 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="first.order", control="control", fixed.cell=TRUE)

res_fo_1z2k <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

oldres_fo_1z2k <- read.table("tests/oldres/oldres_fo_1z2k.dat")
expect_equivalent(res_fo_1z2k, oldres_fo_1z2k)


## 2 K; 1 Z robust 
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.sem", control="control", fixed.cell=TRUE)

res_rob_1z2k <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_rob_1z2k <- read.table("tests/oldres/oldres_rob_1z2k.dat")
expect_equivalent(res_rob_1z2k, oldres_rob_1z2k)



## huber 2 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.huber.white", control="control", fixed.cell=TRUE)

res_hub_1z2k <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_hub_1z2k <- read.table("tests/oldres/oldres_hub_1z2k.dat")
expect_equivalent(res_hub_1z2k, oldres_hub_1z2k)



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
#           data=testdaten, fixed.x=F, group.w.free = TRUE,
#           se="robust")
# summary(m1)


########## Tests with missing values ################

### missing in k

d <- example01
d$k1[2] <- NA

## 1 K; 1 Z missing in k
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_missk <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

oldres_missk <- read.table("tests/oldres/oldres_missk.dat")
expect_equivalent(res_missk, oldres_missk)



#### missing in z
d <- example01
d$z1[10] <- NA

m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_missz <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

oldres_missz <- read.table("tests/oldres/oldres_missz.dat")
expect_equivalent(res_missz, oldres_missz)


#### missing in both k and z
d <- example01
d$k1[2] <- NA
d$z1[10] <- NA

m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 missing="fiml")

res_misszk <- rbind(m1@results@Egx,
                    m1@results@Egxgx,
                    m1@results@Egxgk,
                    m1@results@Egxgxk)

oldres_misszk <- read.table("tests/oldres/oldres_misszk.dat")
expect_equivalent(res_misszk, oldres_misszk)


########## Tests with fixed cell size ################

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", fixed.cell=TRUE)

res_fixedcell <- rbind(m1@results@Egx,
                       m1@results@Egxgx,
                       m1@results@Egxgk,
                       m1@results@Egxgxk)

oldres_fixedcell <- read.table("tests/oldres/oldres_fixedcell.dat")
expect_equivalent(res_fixedcell, oldres_fixedcell)



############ Test with empty cell ################

## should give error message...
d <- subset(example01, subset= !(x=="treat1" & k1=="male"))

expect_error({
  m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                   missing="fiml")
})

############ Test with fixed.z ################

m1 <- effectLite(data=example01, y="dv", z=c("z1","z2"), fixed.cell=TRUE, syntax.only=FALSE,
                 k=c("k1","kateg2"), x="x", fixed.z=TRUE, control="control")

res_fixedz <- rbind(m1@results@Egx,
                    m1@results@Egxgx,
                    m1@results@Egxgk,
                    m1@results@Egxgxk)

oldres_fixedz <- read.table("tests/oldres/oldres_fixedz.dat")
expect_equivalent(res_fixedz, oldres_fixedz)


############ Tests with interaction option ########################

## interaction option none
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="none")

res_int_none <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_int_none <- read.table("tests/oldres/oldres_int_none.dat")
expect_equivalent(res_int_none, oldres_int_none)


## interaction option 2-way
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="2-way")

res_int_2way <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_int_2way <- read.table("tests/oldres/oldres_int_2way.dat")
expect_equivalent(res_int_2way, oldres_int_2way)


## interaction option X:K
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:K")

res_int_xdok <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_int_xdok <- read.table("tests/oldres/oldres_int_xdok.dat")
expect_equivalent(res_int_xdok, oldres_int_xdok)


## interaction option X:Z
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:Z")

res_int_xdoz <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_int_xdoz <- read.table("tests/oldres/oldres_int_xdoz.dat")
expect_equivalent(res_int_xdoz, oldres_int_xdoz)




## interaction option X:K,X:Z
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"),
                 k=c("k1","kateg2"), x="x",
                 control="control",  syntax.only=FALSE,
                 interactions="X:K,X:Z")

res_int_xkxznew <- rbind(m1@results@Egx,
                         m1@results@Egxgx,
                         m1@results@Egxgk,
                         m1@results@Egxgxk)

oldres_int_xkxznew <- read.table("tests/oldres/oldres_int_xkxznew.dat")
expect_equivalent(res_int_xkxznew, oldres_int_xkxznew)



################ homoscedastic residual variances ##############

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", homoscedasticity=TRUE)

res_homosced <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_homosced <- read.table("tests/oldres/oldres_homosced.dat")
expect_equivalent(res_homosced, oldres_homosced)



########## Example with add command ###########

## test with additional conditional effects
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", add="newpar := g200 + g100 \n newpar2 := g100")

res_add_comm <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_add_comm <- read.table("tests/oldres/oldres_add_comm.dat")
expect_equivalent(res_add_comm, oldres_add_comm)




## test with equality constraints
m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 == 0 \n g201 == 0 ",
                 syntax.only=F)

res_equ_cons <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_equ_cons <- read.table("tests/oldres/oldres_equ_cons.dat")
expect_equivalent(res_equ_cons, oldres_equ_cons)




## test with inequality constraint

m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 > 0",
                 syntax.only=F)


res_inequali <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_inequali <- read.table("tests/oldres/oldres_inequali.dat")
expect_equivalent(res_inequali, oldres_inequali)




