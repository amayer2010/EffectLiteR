
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


