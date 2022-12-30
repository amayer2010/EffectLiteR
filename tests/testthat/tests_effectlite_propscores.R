

test_that("elrPredict works with propensity scores",{
  
############ Tests with propensity score ########################

expect_warning({
  m1 <- effectLite(y="y", x="x", propscore=c("z"), control="0",data=nonortho)
})

res_ps_nonortho <- rbind(m1@results@Egx,
                         m1@results@Egxgx,
                         m1@results@Egxgk,
                         m1@results@Egxgxk)

expect_equal(res_ps_nonortho[1,2], 2.717064, tolerance=1e-5)
expect_equal(res_ps_nonortho[3,3], -3.425328, tolerance=1e-5)
expect_equal(res_ps_nonortho[6,5], 0.05680208, tolerance=1e-5)


## propscore with missing value
d <- nonortho
d$z[15] <- NA

expect_warning({
  m1 <- effectLite(y="y", x="x", propscore=c("z"), control="0",data=d)
})

res_ps_na <- rbind(m1@results@Egx,
                   m1@results@Egxgx,
                   m1@results@Egxgk,
                   m1@results@Egxgxk)

expect_equal(res_ps_na[1,2], 2.722922, tolerance=1e-5)
expect_equal(res_ps_na[3,3], -3.406498, tolerance=1e-5)
expect_equal(res_ps_na[6,5], 0.05685217, tolerance=1e-5)



## propscore with formula specification
expect_warning({
  m1 <- effectLite(y="y", x="x", propscore=x~z, control="0", data=d)
})

res_ps_form <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)


expect_equal(res_ps_form[1,3], 0.3387132, tolerance=1e-5)
expect_equal(res_ps_form[4,2], 4.846877, tolerance=1e-5)
expect_equal(res_ps_form[7,5], 0.6717924, tolerance=1e-5)

})


## TODO: variance of propscore too low in this example -- causes error in lavaan
## write informative error message
# m1 <- effectLite(y="dv", z=c("z1"), x="x",
#                  propscore=c("z2"), control="control",data=example01,
#                  syntax.only=FALSE)

