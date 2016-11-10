


########## complex survey design #################

expect_message({
  m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
                   syntax.only=F, data=example_multilevel, 
                   ids=~cid, weights=~weights)
})

res_compsurv <- rbind(m1@results@Egx,
                      m1@results@Egxgx,
                      m1@results@Egxgk,
                      m1@results@Egxgxk)

oldres_compsurv <- read.table("tests/oldres/oldres_compsurv.dat")
expect_equivalent(res_compsurv, oldres_compsurv)



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

oldres_iptw1 <- read.table("tests/oldres/oldres_iptw1.dat")
expect_equivalent(res_iptw1, oldres_iptw1)

## Notice that IPTW only gives unbiased effects for average effects
# m1w@results@Egx
# m1c@results@Egx

## but not for the effects on the treated !!!!
# m1w@results@Egxgx
# m1c@results@Egxgx



m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
                 data=example_multilevel)

res_exmulti <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

oldres_exmulti <- read.table("tests/oldres/oldres_exmulti.dat")
expect_equivalent(res_exmulti, oldres_exmulti)




