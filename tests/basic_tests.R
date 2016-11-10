
## basic tests with different combinations of continuous and categorical covariates

d <- example01

## 1 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1"), x="x", control="control")

res_1k1z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_1k1z <- read.table("tests/oldres/oldres_1k1z.dat")
expect_equivalent(res_1k1z, oldres_1k1z)


## 2 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", control="control")

res_2k1z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_2k1z <- read.table("tests/oldres/oldres_2k1z.dat")
expect_equivalent(res_2k1z, oldres_2k1z)


## 2 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=c("k1","kateg2"), x="x", control="control")

res_2k0z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_2k0z <- read.table("tests/oldres/oldres_2k0z.dat")
expect_equivalent(res_2k0z, oldres_2k0z)


## 0 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k=NULL, x="x", control="control")

res_0k0z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_0k0z <- read.table("tests/oldres/oldres_0k0z.dat")
expect_equivalent(res_0k0z, oldres_0k0z)


## 0 K; 1 Z
m1 <- effectLite(data=d, y="dv", z=c("z1"), k=NULL, x="x", control="control")

res_0k1z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_0k1z <- read.table("tests/oldres/oldres_0k1z.dat")
expect_equivalent(res_0k1z, oldres_0k1z)


## 1 K 0 Z interaction argument
m1 <- effectLite(data=d, y="dv", k=c("k1"), x="x", control="control",
                 interactions="X:Z")

res_1k0zint <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

oldres_1k0zint <- read.table("tests/oldres/oldres_1k0zint.dat")
expect_equivalent(res_1k0zint, oldres_1k0zint)


## 1 K; 0 Z
m1 <- effectLite(data=d, y="dv", z=NULL, k="k1", x="x", control="control")

res_1k0z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_1k0z <- read.table("tests/oldres/oldres_1k0z.dat")
expect_equivalent(res_1k0z, oldres_1k0z)


## 2 K; 2 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2"), 
                 k=c("k1","kateg2"), x="x", control="control")

res_2k2z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_2k2z <- read.table("tests/oldres/oldres_2k2z.dat")
expect_equivalent(res_2k2z, oldres_2k2z)


## 2 K; 3 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3"), 
                 k=c("k1","kateg2"), x="x", control="control")

res_2k3z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_2k3z <- read.table("tests/oldres/oldres_2k3z.dat")
expect_equivalent(res_2k3z, oldres_2k3z)


## 1 K; 4 Z
m1 <- effectLite(data=d, y="dv", z=c("z1","z2","z3","k1"), 
                 k="kateg2", x="x", control="control")

res_1k4z <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_1k4z <- read.table("tests/oldres/oldres_1k4z.dat")
expect_equivalent(res_1k4z, oldres_1k4z)
