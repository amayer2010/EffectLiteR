
####### some tests with latent variables as outcome and/or covariate #######

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

res_lzly <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_lzly <- read.table("tests/oldres/oldres_lzly.dat")
expect_equivalent(res_lzly, oldres_lzly)




## latent and bootstrap
set.seed(142424)

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

oldres_latboot <- read.table("tests/oldres/oldres_latboot.dat")
expect_equivalent(res_latboot, oldres_latboot)




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
oldres_latmf <- read.table("tests/oldres/oldres_latmf.dat")
expect_equivalent(res_latmf, oldres_latmf)




## Steffis method factor 2
mm <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21
mf =~ 1*CPM11 + -1*CPM21 + 1*CPM12 + -1*CPM22

CPM11 + CPM21 + CPM12 + CPM22 ~ 0*1
'

## Wie im alten EffectLite, mit MF als Kovariate:
expect_warning({
  m1 <- effectLite(y="eta2", x="x", z=c("eta1","mf"), control="0", 
                   measurement=mm, data=example02lv)
})

res_latmfsteffi1 <- rbind(m1@results@Egx,
                          m1@results@Egxgx,
                          m1@results@Egxgk,
                          m1@results@Egxgxk)

oldres_latmfsteffi1 <- read.table("tests/oldres/oldres_latmfsteffi1.dat")
expect_equivalent(res_latmfsteffi1, oldres_latmfsteffi1)


## Oder wenn man den MF nicht als Kovariate haben will (sondern nur im Messmodell):
m1 <- effectLite(y="eta2", x="x", z="eta1", control="0", 
                 measurement=mm, data=example02lv)

res_latmfsteffi2 <- rbind(m1@results@Egx,
                          m1@results@Egxgx,
                          m1@results@Egxgk,
                          m1@results@Egxgxk)

oldres_latmfsteffi2 <- read.table("tests/oldres/oldres_latmfsteffi2.dat")
expect_equivalent(res_latmfsteffi2, oldres_latmfsteffi2)


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

oldres_latzandk <- read.table("tests/oldres/oldres_latzandk.dat")
expect_equivalent(res_latzandk, oldres_latzandk)


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

oldres_lzmy <- read.table("tests/oldres/oldres_lzmy.dat")
expect_equivalent(res_lzmy, oldres_lzmy)



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



