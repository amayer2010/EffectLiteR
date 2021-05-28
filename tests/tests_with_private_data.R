
############ Kirchmann Example with latent variable ############

expect_message({
  kirch <- foreign::read.spss("private/data/kirchmann.sav", to.data.frame=T)
})

mmtest <- generateMeasurementModel(names=c("eta2", "eta1"),
                                   indicators=list("eta2"=c("CESD_13","CESD_23"),
                                                   "eta1"=c("CESD_11","CESD_21")),
                                   ncells=2,
                                   model=c("parallel","parallel"))

m1 <- effectLite(y="eta2", x="con_pat", z=c("eta1"), control="controls", 
                 measurement=mmtest, data=kirch, fixed.cell=FALSE,
                 missing="fiml", syntax.only=FALSE)

res_kirch_latent <- rbind(m1@results@Egx,
                          m1@results@Egxgx,
                          m1@results@Egxgk,
                          m1@results@Egxgxk)

oldres_kirch_latent <- read.table("tests/oldres/oldres_kirch_latent.dat")
expect_equivalent(res_kirch_latent, oldres_kirch_latent)



## Kirchmann manifest with propscore
m1 <- effectLite(y="CESD_3", x="con_pat", z=c("CESD_1"), k=c("sex"),
                 control="controls", 
                 data=kirch, fixed.cell=FALSE, 
                 propscore=c("GAQA_1","RSQA_1","RSQV_1"),
                 missing="fiml", syntax.only=FALSE)

res_kirch_manps <- rbind(m1@results@Egx,
                         m1@results@Egxgx,
                         m1@results@Egxgk,
                         m1@results@Egxgxk)

oldres_kirch_manps <- read.table("tests/oldres/oldres_kirch_manps.dat")
expect_equivalent(res_kirch_manps, oldres_kirch_manps)



############ Bettinas Example ############

expect_warning({
  expect_message({
    d <- elrReadData("private/data/Gesamtdatei_Klasse 2 mit Erstsprache.sav") 
  })
})

m1 <- effectLite(data=d, y="b_ELFE_Text", x="Gruppe", 
                 control="KG", propscore=c("Rolle","a_ELFE_Text","a_ELFE_Wort",
                                           "a_ELFE_Speed", "a_ELFE_Satz"))

res_bettina_ps <- rbind(m1@results@Egx,
                        m1@results@Egxgx,
                        m1@results@Egxgk,
                        m1@results@Egxgxk)

oldres_bettina_ps <- read.table("tests/oldres/oldres_bettina_ps.dat")
expect_equivalent(res_bettina_ps, oldres_bettina_ps)


## Bettina simple example
m1 <- effectLite(y="b_ELFE_Text", x="Gruppe", k="Rolle", z="a_ELFE_Text",
                 data=d, control="KG")

res_bettina_simple <- rbind(m1@results@Egx,
                            m1@results@Egxgx,
                            m1@results@Egxgk,
                            m1@results@Egxgxk)

oldres_bettina_simple <- read.table("tests/oldres/oldres_bettina_simple.dat")
expect_equivalent(res_bettina_simple, oldres_bettina_simple)


########## Test with Rolf's problem data (non-convergence in lavaan) ############

expect_message(d <- elrReadData("private/data/problem1.sav"))
# expect_warning(m1 <- effectLite(y="Y", x="X", k="Z", data=d)) ## old lavaan versions
m1 <- effectLite(y="Y", x="X", k="Z", data=d)







