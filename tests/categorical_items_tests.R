

## test categorical items
mm <- generateMeasurementModel(names=c("eta", "xi"), 
                               indicators=list("eta" = paste0("y",1:7,1),
                                                "xi" = paste0("z",1:5,1)), 
                               ncells=2, 
                               model=c("tau-equi-categorical","tau-cong-categorical"), 
                               data=elrdata_categorical_items)
m1 <- effectLite(y="eta", x="x", z="xi", data=elrdata_categorical_items, 
                 measurement=mm, fixed.cell=TRUE)

res_categ_items <- rbind(m1@results@Egx,
                         m1@results@Egxgx)

oldres_categ_items <- read.table("tests/oldres/oldres_categ_items.dat")
expect_equivalent(res_categ_items, oldres_categ_items)


