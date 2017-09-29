
############ elrPredict tests ########################

m1 <- effectLite(y="dv", z=c("z1"), k=c("k1","kateg2"), x="x",
                 control="control", data=example01)

newdata <- data.frame(k1="male", kateg2="1", z1=2)
res_elrpred_1z2k <- elrPredict(m1, newdata)

oldres_elrpred_1z2k <- read.table("tests/oldres/oldres_elrpred_1z2k.dat")
expect_equivalent(res_elrpred_1z2k, oldres_elrpred_1z2k)


# pred <- EffectLiteR:::computeConditionalEffects(m1, newdata=newdata, add.columns=c("covariates"))
# pred



m1 <- effectLite(y="dv", z=c("z1"), x="x", control="control", data=example01)

newdata <- data.frame(z1=2)
res_elrpred_1z0k <- elrPredict(m1, newdata)

oldres_elrpred_1z0k <- read.table("tests/oldres/oldres_elrpred_1z0k.dat")
expect_equivalent(res_elrpred_1z0k, oldres_elrpred_1z0k)



m1 <- effectLite(y="dv", z=c("z1","z2"),
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)
newdata <- data.frame(z1=2, z2=3, k1="male", kateg2="2")
res_elrpred_2z2k <- elrPredict(m1, newdata)

oldres_elrpred_2z2k <- read.table("tests/oldres/oldres_elrpred_2z2k.dat")
expect_equivalent(res_elrpred_2z2k, oldres_elrpred_2z2k)



## compare with method="lm"
m1 <- effectLite(y="dv", z=c("z1","z2"),
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)
m2 <- effectLite(y="dv", z=c("z1","z2"), method="lm",
                 k=c("k1", "kateg2"), x="x", control="control", data=example01)

tmp1 <- round(m1@results@condeffects$g1, 3)
tmp2 <- round(m2@results@condeffects$g1, 3)
expect_equivalent(tmp1,tmp2)


