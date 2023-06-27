

test_that("effectLite works with complex survey design",{
  
########## complex survey design #################

# m1a <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0",
#                   syntax.only=F, data=example_multilevel, ids=~cid, estimator="mlr")  
# 
# m1a <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0",
#                   syntax.only=F, data=example_multilevel,
#                   ids=~cid, weights=~weights)
# m1b <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0",
#                   syntax.only=F, data=example_multilevel)
# m1c <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0",
#                   syntax.only=F, data=example_multilevel,
#                   cluster= "cid", sampling.weights = "weights")
# m1a@results@Egx
# m1b@results@Egx
# m1c@results@Egx

    
expect_message({
  m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
                   syntax.only=F, data=example_multilevel, 
                   ids=~cid, weights=~weights)
})

res_compsurv <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

expect_equal(res_compsurv[1,1], 1.35829, tolerance=1e-5)
expect_equal(res_compsurv[2,3], 6.142792, tolerance=1e-5)
expect_equal(res_compsurv[3,5], 0.9926885, tolerance=1e-5)

})



test_that("effectLite works with propensity score weighting",{
  

########### propensity score weighting ##############

## lavaan.survey only works if there is at least one covariate
## otherwise lavaan.survey:::get.stats.design produces an error, because
## sample.cov.g does not have a var attribute..

expect_message({
  m1 <- effectLite(y="y", x="x", z="weights", fixed.cell=TRUE, control="0", 
                   weights=~iptw, data=example_multilevel)
})

res_iptw1 <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

expect_equal(res_iptw1[2,2], 0.13653, tolerance=1e-5)
expect_equal(res_iptw1[2,3], 9.816644, tolerance=1e-5)
expect_equal(res_iptw1[2,5], 0.8223211, tolerance=1e-5)

actual_names <- names(res_iptw1)
expected_names <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")
expect_equal(actual_names, expected_names)


## Notice that IPTW only gives unbiased effects for average effects
# m1w@results@Egx
# m1c@results@Egx

## but not for the effects on the treated !!!!
# m1w@results@Egxgx
# m1c@results@Egxgx

m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
                 data=example_multilevel)

expect_equal(m1@results@Egx[1,2], 0.111072, tolerance=1e-5)

})


########## latent variables and complex survey design #################

# dlatent <- example_multilevel
# dlatent <- within(dlatent,{
#   y12 <- y + rnorm(800,0,1)
#   y22 <- y + rnorm(800,0,1)
#   z11 <- z + rnorm(800,0,sqrt(0.4))
#   z21 <- z + rnorm(800,0,sqrt(0.4))
# })
# 
# names <- c("eta", "xi")
# indicators <- list("eta" = c("y12","y22"), 
#                    "xi" = c("z11","z21"))
# mm <- generateMeasurementModel(names, indicators, ncells=2)
# 
# m1 <- effectLite(y="eta", x="x", z="xi", fixed.cell=TRUE, control="0", 
#                    data=dlatent, measurement=mm, ids=~cid)
# 
# tmp <- m1@results@lavresults
# 
# 
# res_compsurv <- rbind(m1@results@Egx,
#                       m1@results@Egxgx,
#                       m1@results@Egxgk,
#                       m1@results@Egxgxk)
# 
# oldres_compsurv <- read.table("tests/oldres/oldres_compsurv.dat")
# expect_equivalent(res_compsurv, oldres_compsurv)





