

test_that("effectLite works with many cells",{
  
## tests with many different cells (due to x and/or k)
# d <- read.csv(file="tests/testdata/testdata_many_cells.csv")
  
set.seed(234234)
d <- example01
d$cid <- sample(1:20, size=nrow(d), replace=TRUE)


## many x cells
m1 <- effectLite(data=d, y="dv", x="cid")

res_many_x <- rbind(m1@results@Egx,
                  m1@results@Egxgx,
                  m1@results@Egxgk,
                  m1@results@Egxgxk)

expect_equal(nrow(res_many_x), 399)
expect_equal(res_many_x[275,3], -1.307691, tolerance=1e-5)
expect_equal(names(m1@results@est)[1], "a0_0_0")
expect_equal(m1@results@est[["Eg18gx19"]], -1.740940e-01, tolerance=1e-5)


## many k cells
m1 <- effectLite(data=d, y="dv", k="cid", x="x")

res_many_k <- rbind(m1@results@Egx,
                    m1@results@Egxgx,
                    m1@results@Egxgk,
                    m1@results@Egxgxk)

expect_equal(res_many_k[46,3], 0.6848483, tolerance=1e-5)


## many xk cells and z
m1 <- effectLite(data=d, y="dv", x="cid", k="k1", z="z1")

res_many_xk <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

expect_equal(res_many_xk[698,3], 0.658903, tolerance=1e-5)
expect_equal(nrow(res_many_xk), 1197)


## many xk cells and z with lm
m2 <- effectLite(data=d, y="dv", x="cid", k="k1", z="z1", method="lm")

expect_equivalent(m1@results@Egx$Estimate,
                  m2@results@Egx$Estimate,
                  tolerance=1e-5)

})


