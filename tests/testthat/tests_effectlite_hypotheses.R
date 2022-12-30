

test_that("hypothesis tests work with effectLite",{
  

### Hypothesis tests with SEM #######

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

actual_hypotest_1k1z <- m1@results@hypotheses

expected_hypotest_1k1z <- data.frame(
  x1 = c(0.4781206, 4.9094489, 13.2156255, 13.8240783),
  x2 = c(2, 3, 6, 8),
  x3 = c(0.78736738, 0.17854910, 0.03973676, 0.08646756)
)
names(expected_hypotest_1k1z) <- c("Wald Chi-Square", "df", "p-value")
row.names(expected_hypotest_1k1z) <- c("No average effects",
                                       "No covariate effects in control group",
                                       "No treatment*covariate interaction",
                                       "No treatment effects")

expect_equal(actual_hypotest_1k1z, expected_hypotest_1k1z,
             tolerance=1e-5)


## 1 K
m1 <- effectLite(data=example01, y="dv", k=c("k1"), x="x", control="control")
res_hypotest_1k <- m1@results@hypotheses

expect_equal(res_hypotest_1k$`Wald Chi-Square`[1], 0.5727793, tolerance=1e-5)

## no covs
m1 <- effectLite(data=example01, y="dv", x="x", control="control")

expect_equal(nrow(m1@results@hypotheses), 1)


### Hypothesis tests with lm #######

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", method="lm")
actual_hypotest_1k1z_lm <- m1@results@hypotheses

expected_hypotest_1k1z_lm <- data.frame(
  x1 = c(0.2466545, 1.5214327, 2.1739288, 1.7097509),
  x2 = c(2, 3, 6, 8),
  x3 = c(1988, 1988, 1988, 1988),
  x4 = c(0.78143452, 0.20698192, 0.04281359, 0.09130724)
)
names(expected_hypotest_1k1z_lm) <- c("F value", "df1", "df2", "p-value")
row.names(expected_hypotest_1k1z_lm) <- c("No average effects",
                                          "No covariate effects in control group",
                                          "No treatment*covariate interaction",
                                          "No treatment effects")

expect_equal(actual_hypotest_1k1z_lm,
             expected_hypotest_1k1z_lm,
             tolerance=1e-5)


## 1 K
m1 <- effectLite(data=example01, y="dv", k=c("k1"), x="x", control="control", method="lm")
expect_equal(ncol(m1@results@hypotheses), 4)


## no covs
m1 <- effectLite(data=example01, y="dv", x="x", control="control", method="lm")
expect_equal(nrow(m1@results@hypotheses), 1)


######## Hypothesis tests with SEM and interactions ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:K")
res_hypotest_xk_int <- m1@results@hypotheses

expect_equal(res_hypotest_xk_int[1,3], 0.75443424, tolerance=1e-5)
expect_equal(res_hypotest_xk_int[4,1], 8.1601952, tolerance=1e-5)

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")
expect_equal(m1@results@hypotheses[4,1], 1.2273848, tolerance=1e-5)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="no")

expect_true(is.na(m1@results@hypotheses[3,1]))


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="all")
expect_equal(m1@results@hypotheses[4,1], 13.8240783, tolerance=1e-5)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), x="x", control="control",
                 interactions="X:K", se="robust.sem", fixed.cell=TRUE)
expect_true(is.na(m1@results@hypotheses[3,1]))
expect_equal(m1@results@hypotheses[4,1], 0.624142416, tolerance=1e-5)


######## Hypothesis tests with lm and interactions ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:K", method="lm")

m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")
expect_equal(m1@results@hypotheses[2,2], 3)


m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="no")
expect_true(is.na(m1@results@hypotheses[3,1]))
expect_equal(m1@results@hypotheses[1,1], m1@results@hypotheses[4,1])


######## K-Conditional Hypothesis tests ##########

## 1 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")
expect_equal(m1@results@hypothesesk[1,1], 5.520539, tolerance=1e-5)
expect_equal(nrow(m1@results@hypothesesk), 2)


## 2 K
m1 <- effectLite(data=example01, y="dv", k=c("k1","kateg2"), x="x", control="control")
expect_equal(nrow(m1@results@hypothesesk), 4)


## 1 K; 1 Z with lm
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 method="lm")
expect_equal(nrow(m1@results@hypothesesk), 2)
expect_equal(m1@results@hypothesesk[2,4], 0.24958327, tolerance=1e-5)

## 2 K with lm
m1 <- effectLite(data=example01, y="dv", k=c("k1","kateg2"), x="x", control="control",
                 method="lm")
expect_equal(m1@results@hypothesesk[2,4], 0.2145218, tolerance=1e-5)

## 1 K; 1 Z with interaction
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", control="control",
                 interactions="X:Z")

})


