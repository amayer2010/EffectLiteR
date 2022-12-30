
test_that("effectLite works with latent z and y",{
  
## latent z and latent y

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

effectsl <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

expect_equal(effectsl[1,1], 1.901696, tolerance=1e-5)
expect_equal(effectsl[2,3], 11.07721, tolerance=1e-5)
expect_equal(effectsl[3,5], 2.154411, tolerance=1e-5)

})




test_that("effectLite works with latent variables and bootstrap",{
  

## latent and bootstrap
set.seed(142424)

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m)*1
CPM22 ~ c(p,p)*1
'
  
  
expect_warning({
  m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
                   measurement=mmtest, data=example02lv, fixed.cell=TRUE,
                   missing="fiml", syntax.only=FALSE,
                   se="boot", bootstrap=5L)
})

res_latboot <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)


expect_equal(res_latboot[1,1], 1.901696, tolerance=1e-5)
expect_equal(res_latboot[2,3], 12.95835, tolerance=1e-5)
expect_equal(res_latboot[3,5], 2.154411, tolerance=1e-5)

})




test_that("effectLite works with method factors",{
  

############ Example 01a with method factor ################## 

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21
mf =~ 1*CPM11 + 1*CPM12

CPM11 + CPM21 + CPM12 + CPM22 ~ 0*1
'

expect_warning({
  m1 <- effectLite(y="eta2", x="x", z=c("eta1","mf"), control="0", 
                   measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                   missing="fiml", syntax.only=FALSE)
})

res_latmf <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

expect_equal(res_latmf[2,2], 0.8210409, tolerance=1e-5)
expect_equal(res_latmf[2,4], 0.0086504805, tolerance=1e-5)
expect_equal(res_latmf[3,5], 2.163089, tolerance=1e-5)


## Steffis method factor 2
mm <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21
mf =~ 1*CPM11 + -1*CPM21 + 1*CPM12 + -1*CPM22

CPM11 + CPM21 + CPM12 + CPM22 ~ 0*1
'

## As in Ivailo Partchev's EffectLite with MF as covariate
expect_warning({
  m1 <- effectLite(y="eta2", x="x", z=c("eta1","mf"), control="0", 
                   measurement=mm, data=example02lv)
})

res_latmfsteffi1 <- rbind(m1@results@Egx,
                          m1@results@Egxgx,
                          m1@results@Egxgk,
                          m1@results@Egxgxk)

expect_equal(res_latmfsteffi1[2,2], 0.8167239, tolerance=1e-5)
expect_equal(res_latmfsteffi1[2,4], 0.008304169, tolerance=1e-5)
expect_equal(res_latmfsteffi1[3,5], 2.198004, tolerance=1e-5)


## Oder wenn man den MF nicht als Kovariate haben will (sondern nur im Messmodell):
m1 <- effectLite(y="eta2", x="x", z="eta1", control="0", 
                 measurement=mm, data=example02lv)

res_latmfsteffi2 <- rbind(m1@results@Egx,
                          m1@results@Egxgx,
                          m1@results@Egxgk,
                          m1@results@Egxgxk)

expect_equal(res_latmfsteffi2[2,2], 0.11576991, tolerance=1e-5)
expect_equal(res_latmfsteffi2[2,3], 11.69961, tolerance=1e-5)
expect_equal(res_latmfsteffi2[3,5], 2.078501, tolerance=1e-5)

})




test_that("effectLite works with latent variable and K",{
  
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

res_latzandk <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_latzandk[3,3], 23.01217, tolerance=1e-5)
expect_equal(res_latzandk[5,3], 13.67339, tolerance=1e-5)
expect_equal(res_latzandk[7,5], 0.9992641, tolerance=1e-5)

})



test_that("effectLite works with latent z and manifest y or z2",{
  
######### Example with latent z and manifest y #############

mmtest <- '
eta1 =~ 1*CPM11 + 1*CPM21
CPM11~ 0*1
CPM21 ~ c(m,m)*1
'

m1 <- effectLite(y="CPM22", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)

res_lzmy <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

expect_equal(res_lzmy[1,1], 1.805862, tolerance=1e-5)
expect_equal(res_lzmy[2,2], 0.1289066, tolerance=1e-5)
expect_equal(res_lzmy[3,3], 20.93596, tolerance=1e-5)


######### Example with latent z and manifest z #############

d <- example02lv
d$maniz <- rnorm(nrow(d))

mmtest <- '
eta1 =~ 1*CPM11 + 1*CPM21
CPM11~ 0*1
CPM21 ~ c(m,m)*1
'

m1 <- effectLite(y="CPM22", x="x", z=c("eta1","maniz"), control="0", 
                 measurement=mmtest, data=d, fixed.cell=TRUE,
                 missing="fiml", syntax.only=FALSE)

})

