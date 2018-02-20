

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



######## Hypothesis tests with SEM and interactions ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:K")
res_hypotest_xk_int <- m1@results@hypotheses
oldres_hypotest_xk_int <- readRDS("tests/oldres/oldres_hypotest_xk_int.rds")
expect_equivalent(res_hypotest_xk_int, oldres_hypotest_xk_int)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")
res_hypotest_xz_int <- m1@results@hypotheses
oldres_hypotest_xz_int <- readRDS("tests/oldres/oldres_hypotest_xz_int.rds")
expect_equivalent(res_hypotest_xz_int, oldres_hypotest_xz_int)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="no")
res_hypotest_no_int <- m1@results@hypotheses
oldres_hypotest_no_int <- readRDS("tests/oldres/oldres_hypotest_no_int.rds")
expect_equivalent(res_hypotest_no_int, oldres_hypotest_no_int)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="all")
res_hypotest_all_int <- m1@results@hypotheses
oldres_hypotest_all_int <- readRDS("tests/oldres/oldres_hypotest_all_int.rds")
expect_equivalent(res_hypotest_all_int, oldres_hypotest_all_int)



m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", control="control",
                 interactions="X:K", se="robust.sem", fixed.cell=TRUE)
res_hypotest_xkrob_int <- m1@results@hypotheses
oldres_hypotest_xkrob_int <- readRDS("tests/oldres/oldres_hypotest_xkrob_int.rds")
expect_equivalent(res_hypotest_xkrob_int, oldres_hypotest_xkrob_int)





######## Hypothesis tests with lm and interactions ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:K", method="lm")

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")
res_hypotest_xz_int <- m1@results@hypotheses
oldres_hypotest_xz_int <- readRDS("tests/oldres/oldres_hypotest_xz_int.rds")
expect_equivalent(res_hypotest_xz_int, oldres_hypotest_xz_int)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="no")
res_hypotest_no_int <- m1@results@hypotheses
oldres_hypotest_no_int <- readRDS("tests/oldres/oldres_hypotest_no_int.rds")
expect_equivalent(res_hypotest_no_int, oldres_hypotest_no_int)




######## K-Conditional Hypothesis tests ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")
res_kcondhypotest_kz <- m1@results@hypothesesk
oldres_kcondhypotest_kz <- readRDS("tests/oldres/oldres_kcondhypotest_kz.rds")
expect_equivalent(res_kcondhypotest_kz, oldres_kcondhypotest_kz)


## 2 K
m1 <- effectLite(data=example01, y="dv", k=c("k1","kateg2"), x="x", control="control")
res_kcondhypotest_2k <- m1@results@hypothesesk
oldres_kcondhypotest_2k <- readRDS("tests/oldres/oldres_kcondhypotest_2k.rds")
expect_equivalent(res_kcondhypotest_2k, oldres_kcondhypotest_2k)


## 1 K; 1 Z with lm
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 method="lm")
res_kcondhypotest_kz_lm <- m1@results@hypothesesk
oldres_kcondhypotest_kz_lm <- readRDS("tests/oldres/oldres_kcondhypotest_kz_lm.rds")
expect_equivalent(res_kcondhypotest_kz_lm, oldres_kcondhypotest_kz_lm)


## 2 K with lm
m1 <- effectLite(data=example01, y="dv", k=c("k1","kateg2"), x="x", control="control",
                 method="lm")
res_kcondhypotest_2k_lm <- m1@results@hypothesesk
oldres_kcondhypotest_2k_lm <- readRDS("tests/oldres/oldres_kcondhypotest_2k_lm.rds")
expect_equivalent(res_kcondhypotest_2k_lm, oldres_kcondhypotest_2k_lm)


## 1 K; 1 Z with interaction
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")
test <- m1@results@hypothesesk
