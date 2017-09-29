
## basic tests with method="lm"

d <- example01

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", 
                 control="control", method="lm", interaction="all")

## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                 control="control", method="lm")

## 2 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=c("k1","kateg2"), x="x", 
                 method="lm", control="control")
m2 <- effectLite(data=d, y="dv", z=NULL, k=c("k1","kateg2"), x="x", fixed.cell=TRUE,
                 method="sem", control="control", homoscedasticity=TRUE)

expect_equivalent(round(m1@results@Egxgk$Estimate, 4),
                  round(m2@results@Egxgk$Estimate, 4)) 

## 0 K; 0 Z
m1 <- effectLite(data=d, y="dv", method="lm", z=NULL, k=NULL, x="x", control="control")


## 0 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=NULL, x="x", control="control",
                 method="lm")


## 2 K; 3 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3"), 
                 k=c("k1","kateg2"), x="x", control="control",
                 method="lm")

m2 <- effectLite(data=d, y="dv", z=c("z1","z2","z3"), 
                 k=c("k1","kateg2"), x="x", control="control",
                 method="sem", homoscedasticity=TRUE)

expect_equivalent(round(m1@results@Egxgxk$Estimate, 3),
                  round(m2@results@Egxgxk$Estimate, 3)) 


## compare with Anova (in the balanced case)

set.seed(324)

N <- 150
design <- expand.grid(k=0:2, x=0:1)
design$y <- c(40,30,70,50,20,80)
ind <- rep(1:6, each=25)
d <- design[ind,]
d$y <- d$y + rnorm(N,0,5)
d$x <- as.factor(d$x)
d$k <- as.factor(d$k)

options(contrasts=c("contr.treatment","contr.poly"))

m1 <- effectLite(y="y", x="x", k="k", control="0", data=d, 
                 fixed.cell=TRUE, method="sem", homoscedasticity=TRUE)

m2 <- effectLite(y="y", x="x", k="k", control="0", data=d, method="lm")

options(contrasts=c("contr.sum","contr.poly"))
m3 <- lm(y ~ x*k, data=d)


tmp0 <- m1@results@hypotheses[[1,1]] ## Wald chisquare
tmp1 <- m2@results@hypotheses[[1,1]] ## F value
tmp2 <- Anova(m3, type="III")[2,3]

expect_equivalent(round(tmp0,5),10.15771)
expect_equivalent(tmp1,tmp2) ## main effect x and p value

tmp1 <- m2@results@hypotheses[[1,4]] ## p values
tmp2 <- Anova(m3, type="III")[2,4]
expect_equivalent(tmp1,tmp2)

tmp1 <- m2@results@hypotheses[[3,1]] ## interaction term
tmp2 <- Anova(m3, type="III")[4,3]
expect_equivalent(tmp1,tmp2)

tmp1 <- m2@results@hypotheses[[3,4]] ## p value interaction term
tmp2 <- Anova(m3, type="III")[4,4]
expect_equivalent(tmp1,tmp2)


options(contrasts=c("contr.treatment","contr.poly"))

