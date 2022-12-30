

test_that("effectLite works with non-standard SE",{
  
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

expect_equal(res_boot5_1z2k[5,3], -0.4216187, tolerance=1e-5)
expect_equal(res_boot5_1z2k[40,5], 0.2281321, tolerance=1e-5)


## first order SE 2 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="first.order", control="control", fixed.cell=TRUE)

res_fo_1z2k <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)


expect_equal(res_fo_1z2k[5,3], -0.3244251, tolerance=1e-5)
expect_equal(res_fo_1z2k[40,5], 0.2281321, tolerance=1e-5)


## 2 K; 1 Z robust 
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.sem", control="control", fixed.cell=TRUE)

res_rob_1z2k <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_rob_1z2k[6,2], 0.05435203, tolerance=1e-5)
expect_equal(res_rob_1z2k[8,2], 0.05418127, tolerance=1e-5)


## huber 2 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 se="robust.huber.white", control="control", fixed.cell=TRUE)

res_hub_1z2k <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_hub_1z2k[6,3], 0.1910645, tolerance=1e-5)
expect_equal(res_hub_1z2k[9,5], -0.1191425, tolerance=1e-5)



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

})



test_that("effectLite works with missing options",{
  
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

expect_equal(res_missk[6,3], 0.3542112, tolerance=1e-5)
expect_equal(res_missk[9,5], -0.1584472, tolerance=1e-5)


#### missing in z
d <- example01
d$z1[10] <- NA

m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_missz <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

expect_equal(res_missz[6,3], 0.3442177, tolerance=1e-5)
expect_equal(res_missz[9,5], -0.1584472, tolerance=1e-5)


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

expect_equal(res_misszk[3,5], -0.001094094, tolerance=1e-5)
expect_equal(res_misszk[12,2], 0.07711113, tolerance=1e-5)

})




test_that("effectLite works with fixed cell size",{
  

########## Tests with fixed cell size ################

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", fixed.cell=TRUE)

res_fixedcell <- rbind(m1@results@Egx,
                       m1@results@Egxgx,
                       m1@results@Egxgk,
                       m1@results@Egxgxk)

expect_equal(res_fixedcell[4,5], 0.03102239, tolerance=1e-5)
expect_equal(res_fixedcell[17,2], 0.0794723, tolerance=1e-5)

})



test_that("effectLite works with some special cases",{
  
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

expect_equal(res_fixedz[4,5], 0.01759279, tolerance=1e-5)
expect_equal(res_fixedz[17,2], 0.1094813, tolerance=1e-5)

})


test_that("effectLite's interaction option works",{
  
  
############ Tests with interaction option ########################

## interaction option none
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="none")

res_int_none <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_int_none[7,5], -0.006986638, tolerance=1e-5)
expect_equal(res_int_none[18,2], 0.05328545, tolerance=1e-5)


## interaction option 2-way
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="2-way")

res_int_2way <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_int_2way[7,5], -0.02455052, tolerance=1e-5)
expect_equal(res_int_2way[18,2], 0.07394931, tolerance=1e-5)


## interaction option X:K
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:K")

res_int_xdok <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_int_xdok[8,2], 0.05332877, tolerance=1e-5)
expect_equal(res_int_xdok[19,3], -1.007574, tolerance=1e-5)


## interaction option X:Z
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", 
                 control="control",  syntax.only=FALSE,
                 interactions="X:Z")

res_int_xdoz <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_int_xdoz[8,2], 0.05352164, tolerance=1e-5)
expect_equal(res_int_xdoz[19,3], -0.1293819, tolerance=1e-5)



## interaction option X:K,X:Z
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"),
                 k=c("k1","kateg2"), x="x",
                 control="control",  syntax.only=FALSE,
                 interactions="X:K,X:Z")

res_int_xkxznew <- rbind(m1@results@Egx,
                         m1@results@Egxgx,
                         m1@results@Egxgk,
                         m1@results@Egxgxk)

expect_equal(res_int_xkxznew[8,2], 0.05320093, tolerance=1e-5)
expect_equal(res_int_xkxznew[19,3], -1.039869, tolerance=1e-5)

## interaction option no
m1 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"),
                 k=c("k1","kateg2"), x="x", homoscedasticity=TRUE,
                 control="control",  syntax.only=FALSE,
                 interactions="no")

m2 <- effectLite(fixed.cell = TRUE, data=example01, y="dv", z=c("z1","z2"),
                 k=c("k1","kateg2"), x="x", method="lm",
                 control="control",  syntax.only=FALSE,
                 interactions="no")

expect_equal(m1@results@Egxgk$Estimate, 
             m2@results@Egxgk$Estimate,
             tolerance=1e-5)

})



test_that("effectLite's homoscedastic residual variances option works",{
  
################ homoscedastic residual variances ##############

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", homoscedasticity=TRUE)

res_homosced <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_homosced[8,2], 0.05377823, tolerance=1e-5)
expect_equal(res_homosced[19,3], 1.64309, tolerance=1e-5)

})



test_that("effectLite works with additional syntax",{
  
########## Example with add command ###########

## test with additional conditional effects
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", add="newpar := g200 + g100 \n newpar2 := g100")

expect_equal(m1@results@AdditionalEffects[1,2], 0.1313536, tolerance=1e-5)
expect_equal(m1@results@AdditionalEffects[2,5], -0.1588459, tolerance=1e-5)


## test with equality constraints
m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 == 0 \n g201 == 0 ",
                 syntax.only=F)

expect_equal(m1@results@Egx[1,1], -0.009915456, tolerance=1e-5)
expect_equal(m1@results@est[["g101"]], 0, tolerance=1e-5)


## test with inequality constraint

m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", 
                 control="control", add="g101 > 0",
                 syntax.only=F)

expect_equal(m1@results@Egx[1,1], -0.01001208, tolerance=1e-5)
expect_equal(m1@results@est[["g101"]], 0, tolerance=1e-5)



## test with additional conditional effects and lm
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", add="newpar := g200 + g100 \n newpar2 := g100")
m2 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", method="lm",
                 control="control", add="newpar := g200 + g100 \n newpar2 := g100")


expect_equal(m1@results@AdditionalEffects$Estimate, 
             m2@results@AdditionalEffects$Estimate, 
             tolerance=1e-5)

## equality constraints should not work with lm
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", method="sem",
                 control="control", add="newpar := g200 + g100 \n g200 == g100")

expect_error({
m2 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", method="lm",
                 control="control", add="newpar := g200 + g100 \n g200 == g100")
})


})

