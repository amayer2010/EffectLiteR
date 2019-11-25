

## adjmeans 2 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), 
                 x="x", control="control")

res_adjmeans_2k1z <- m1@results@adjmeans
oldres_adjmeans_2k1z <- read.table("tests/oldres/oldres_adjmeans_2k1z.dat")
expect_equivalent(res_adjmeans_2k1z, oldres_adjmeans_2k1z)


## 0 K; 0 Z
m1 <- effectLite(data=example01, y="dv", z=NULL, k=NULL, x="x", 
                 control="control")

res_adjmeans_0k0z <- m1@results@adjmeans
oldres_adjmeans_0k0z <- read.table("tests/oldres/oldres_adjmeans_0k0z.dat")
expect_equivalent(res_adjmeans_0k0z, oldres_adjmeans_0k0z)


## 0 K; 1 Z
m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=NULL, x="x", 
                 control="control")

res_adjmeans_0k1z <- m1@results@adjmeans
oldres_adjmeans_0k1z <- read.table("tests/oldres/oldres_adjmeans_0k1z.dat")
expect_equivalent(res_adjmeans_0k1z, oldres_adjmeans_0k1z)

