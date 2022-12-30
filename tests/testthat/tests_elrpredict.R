

test_that("elrPredict works",{
  
############ elrPredict tests ########################

m1 <- effectLite(y="dv", z=c("z1"), k=c("k1","kateg2"), x="x",
                 control="control", data=example01)

newdata <- data.frame(k1="male", kateg2="1", z1=2)

actual_elrpred_1z2k <- elrPredict(m1, newdata)
expected_elrpred_1z2k <- as.data.frame(rbind(c(-0.09468844, 0.2482164, -0.2439777, 
        0.2400799, 0.1850224, 0.09033398, -0.0589553)))
names(expected_elrpred_1z2k) <- c("g1", "se_g1", "g2", "se_g2", "ExpOutc0",
                                  "ExpOutc1", "ExpOutc2")
row.names(expected_elrpred_1z2k) <- 1

expect_equal(actual_elrpred_1z2k, expected_elrpred_1z2k, tolerance=1e-5)


# pred <- EffectLiteR:::computeConditionalEffects(m1, newdata=newdata, add.columns=c("covariates"))
# pred


m1 <- effectLite(y="dv", z=c("z1"), x="x", control="control", data=example01)

newdata <- data.frame(z1=2)
res_elrpred_1z0k <- elrPredict(m1, newdata)

expect_equal(res_elrpred_1z0k[1,1], -0.07810149, tolerance=1e-5)
expect_equal(res_elrpred_1z0k[1,4], 0.1202491, tolerance=1e-5)
expect_equal(res_elrpred_1z0k[1,6], -0.01161097, tolerance=1e-5)


m1 <- effectLite(y="dv", z=c("z1","z2"),
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)
newdata <- data.frame(z1=2, z2=3, k1="male", kateg2="2")
res_elrpred_2z2k <- elrPredict(m1, newdata)

expect_equal(res_elrpred_2z2k[1,1], -0.5321588, tolerance=1e-5)
expect_equal(res_elrpred_2z2k[1,4], 0.3748164, tolerance=1e-5)
expect_equal(res_elrpred_2z2k[1,6], -0.2983146, tolerance=1e-5)


})


test_that("elrPredict works with lm and sem",{
  
## compare with method="lm"
m1 <- effectLite(y="dv", z=c("z1","z2"),
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)
m2 <- effectLite(y="dv", z=c("z1","z2"), method="lm",
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)

tmp1 <- m1@results@condeffects$g1
tmp2 <- m2@results@condeffects$g1
expect_equal(tmp1,tmp2, tolerance=1e-5)

})
