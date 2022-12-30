

test_that("effectLite aggregated effects works",{

## basic tests with different combinations of continuous and categorical covariates

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 fixed.z=TRUE, fixed.cell=TRUE)

m2 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 fixed.z=TRUE, fixed.cell=TRUE, method="lm")


## effect given X and K
newdata <- data.frame(k1="male", z1=NA, x="control")
agg.subset <- autoSelectSubset(m1, newdata, nsub=10)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@Egxgxk

expect_equal(opt1[["Agg_g1"]], opt2$Estimate[1], tolerance=1e-5)

# with lm
opt3 <- computeAggregatedEffects(m2, agg.subset=agg.subset)

expect_equal(opt1[c(1,3,5,6,7)],  ## sem
             opt3[c(1,3,5,6,7)],  ## lm
             tolerance=1e-5)

## average effect
newdata <- data.frame(k1=NA, z1=NA, x=NA)
agg.subset <- autoSelectSubset(m1, newdata, nsub=10)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@Egx

expect_equal(opt1[["Agg_g1"]], opt2$Estimate[1])

## k conditional effect
newdata <- data.frame(k1="female", z1=NA, x=NA)
agg.subset <- autoSelectSubset(m1, newdata, nsub=10)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@Egxgk

expect_equal(opt1[["Agg_g2"]], opt2$Estimate[4])

## Z=1 conditional effect
newdata <- data.frame(k1=NA, z1=1)
agg.subset <- autoSelectSubset(m1, newdata, nsub=10)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)

expect_equal(opt1[["Agg_g1"]], -0.15667805, tolerance=1e-5)

})


test_that("comparison with elrPredict works",{
  
## compare with elrPredict

m1 <- effectLite(data=example01, y="dv", z=c("z1","z2"), k=c("k1","kateg2"), x="x", 
                 control="control", fixed.z=TRUE, fixed.cell=TRUE)

newdata <- data.frame(k1=NA, z1=1)
expect_error(agg.subset <- autoSelectSubset(m1, newdata, nsub=10))

newdata <- data.frame(k1="female", z1=1, kateg2="1", z2=NA, x=NA)
agg.subset <- autoSelectSubset(m1, newdata, nsub=1)

opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@condeffects[agg.subset,]

expect_equal(opt1[["Agg_g1"]], opt2$g1)

})


test_that("example with latent variable works",{
  
## test with latent variable

mmtest <- '
eta2 =~ 1*CPM12 + 1*CPM22
eta1 =~ 1*CPM11 + 1*CPM21

CPM11 + CPM12 ~ 0*1

CPM21 ~ c(m,m)*1
CPM22 ~ c(p,p)*1
'

m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
                 measurement=mmtest, data=example02lv, fixed.cell=TRUE)

newdata <- data.frame(eta1=1.345499)
agg.subset <- autoSelectSubset(m1, newdata, nsub=1)

opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@condeffects[agg.subset,]
opt3 <- elrPredict(m1, newdata) 

expect_equal(opt1[["Agg_g1"]], opt2$g1)
expect_equal(round(opt2$g1,5), round(opt3$g1,5))

})


test_that("missing data works",{
  
## test with missing data

dmis <- example01
dmis$z1[1:30] <- dmis$k1[100:400] <- NA

m1 <- effectLite(data=dmis, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 fixed.z=TRUE, fixed.cell=TRUE, missing="listwise")

newdata <- data.frame(k1="male", z1=NA)

agg.subset <- autoSelectSubset(m1, newdata)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@Egxgk

expect_equivalent(opt1[["Agg_g1"]], 
                  opt2$Estimate[1],
                  tolerance=1e-4) ## small differences, negligible?

})



test_that("interaction and lm works",{
  
######### test with interaction and lm ###########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 fixed.z=TRUE, fixed.cell=TRUE, interactions="X:K")

m2 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 fixed.z=TRUE, fixed.cell=TRUE, method="lm", interactions="X:K")


## effect given X and K
newdata <- data.frame(k1="male", z1=NA, x="control")
agg.subset <- autoSelectSubset(m1, newdata, nsub=10)
opt1 <- computeAggregatedEffects(m1, agg.subset=agg.subset)
opt2 <- m1@results@Egxgxk

expect_equal(opt1[["Agg_g1"]], opt2$Estimate[1])

# with lm
opt3 <- computeAggregatedEffects(m2, agg.subset=agg.subset)

expect_equal(opt1[c(1,3,5,6,7)],  ## sem
             opt3[c(1,3,5,6,7)],  ## lm
             tolerance=1e-3)      ## needs more tolerance

})

