
## tests with many different cells (due to x and/or k)

d <- read.csv(file="tests/testdata/testdata_many_cells.csv")


## many x cells
m1 <- effectLite(data=d, y="dv", x="cid")

res_many_x <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

oldres_many_x <- read.table("tests/oldres/oldres_many_x.dat")
expect_equivalent(res_many_x, oldres_many_x)


## many k cells
m1 <- effectLite(data=d, y="dv", k="cid", x="x")

res_many_k <- rbind(m1@results@Egx,
                    m1@results@Egxgx,
                    m1@results@Egxgk,
                    m1@results@Egxgxk)

oldres_many_k <- read.table("tests/oldres/oldres_many_k.dat")
expect_equivalent(res_many_k, oldres_many_k)


## many xk cells and z
m1 <- effectLite(data=d, y="dv", x="cid", k="k1", z="z1")

res_many_xk <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

oldres_many_xk <- read.table("tests/oldres/oldres_many_xk.dat")
expect_equivalent(res_many_xk, oldres_many_xk)

