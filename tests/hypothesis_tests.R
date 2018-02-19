

### Hypothesis tests with SEM #######


## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_hypotest_1k1z <- m1@results@hypotheses
oldres_hypotest_1k1z <- readRDS("tests/oldres/oldres_hypotest_1k1z.rds")
expect_equivalent(res_hypotest_1k1z, oldres_hypotest_1k1z)


## 1 K
m1 <- effectLite(data=example01, y="dv", k=c("k1"), x="x", control="control")

res_hypotest_1k <- m1@results@hypotheses
oldres_hypotest_1k <- readRDS("tests/oldres/oldres_hypotest_1k.rds")
expect_equivalent(res_hypotest_1k, oldres_hypotest_1k)

## no covs
m1 <- effectLite(data=example01, y="dv", x="x", control="control")

res_hypotest_x <- m1@results@hypotheses
oldres_hypotest_x <- readRDS("tests/oldres/oldres_hypotest_x.rds")
expect_equivalent(res_hypotest_x, oldres_hypotest_x)



### Hypothesis tests with lm #######


## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", method="lm")

res_hypotest_1k1z_lm <- m1@results@hypotheses
oldres_hypotest_1k1z_lm <- readRDS("tests/oldres/oldres_hypotest_1k1z_lm.rds")
expect_equivalent(res_hypotest_1k1z_lm, oldres_hypotest_1k1z_lm)


## 1 K
m1 <- effectLite(data=example01, y="dv", k=c("k1"), x="x", control="control", method="lm")

res_hypotest_1k_lm <- m1@results@hypotheses
oldres_hypotest_1k_lm <- readRDS("tests/oldres/oldres_hypotest_1k_lm.rds")
expect_equivalent(res_hypotest_1k_lm, oldres_hypotest_1k_lm)



## no covs
m1 <- effectLite(data=example01, y="dv", x="x", control="control", method="lm")

res_hypotest_x_lm <- m1@results@hypotheses
oldres_hypotest_x_lm <- readRDS("tests/oldres/oldres_hypotest_x_lm.rds")
expect_equivalent(res_hypotest_x_lm, oldres_hypotest_x_lm)


