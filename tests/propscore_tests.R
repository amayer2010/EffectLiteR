
############ Tests with propensity score ########################


expect_warning({
  m1 <- effectLite(y="y", x="x", propscore=c("z"), control="0",data=nonortho)
})

res_ps_nonortho <- rbind(m1@results@Egx,
                         m1@results@Egxgx,
                         m1@results@Egxgk,
                         m1@results@Egxgxk)

oldres_ps_nonortho <- read.table("tests/oldres/oldres_ps_nonortho.dat")
expect_equivalent(res_ps_nonortho, oldres_ps_nonortho)


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

oldres_ps_na <- read.table("tests/oldres/oldres_ps_na.dat")
expect_equivalent(res_ps_na, oldres_ps_na)


## propscore with formula specification
expect_warning({
  m1 <- effectLite(y="y", x="x", propscore=x~z, control="0", data=d)
})

res_ps_form <- rbind(m1@results@Egx,
                     m1@results@Egxgx,
                     m1@results@Egxgk,
                     m1@results@Egxgxk)

oldres_ps_form <- read.table("tests/oldres/oldres_ps_form.dat")
expect_equivalent(res_ps_form, oldres_ps_form)



## TODO: variance of propscore too low in this example -- causes error in lavaan
## write informative error message
# m1 <- effectLite(y="dv", z=c("z1"), x="x",
#                  propscore=c("z2"), control="control",data=example01,
#                  syntax.only=FALSE)

