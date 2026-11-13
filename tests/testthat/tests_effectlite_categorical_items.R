

test_that("effectLite works with categorical items",{

## test categorical items
mm <- generateMeasurementModel(names=c("eta", "xi"), 
                               indicators=list("eta" = paste0("y",1:7,1),
                                                "xi" = paste0("z",1:5,1)), 
                               ncells=2, 
                               model=c("tau-equi-categorical","tau-cong-categorical"), 
                               data=elrdata_categorical_items)
m1 <- effectLite(y="eta", x="x", z="xi", data=elrdata_categorical_items, 
                 measurement=mm, fixed.cell=TRUE)

m1 <- effectLite(y="eta", x="x", z="xi", data=elrdata_categorical_items, 
                 measurement=mm, fixed.cell=FALSE)

actual_effects_categ_items <- rbind(m1@results@Egx,
                                    m1@results@Egxgx)

expected_effects_categ_items <- data.frame(rbind(
  c(0.11067810, 0.10492001, 1.0548807, 0.291479876, 0.1645116),
  c(0.27555281, 0.08563549, 3.2177409, 0.001292045, 0.4095809),
  c(0.02873438, 0.13541874, 0.2121891, 0.831959500, 0.0427107)
))

names(expected_effects_categ_items) <- c("Estimate", "SE", "Est./SE", "p-value",
                                         "Effect Size")
row.names(expected_effects_categ_items) <- c("Eg1", "Eg1gx0", "Eg1gx1")

expect_equal(actual_effects_categ_items,
             expected_effects_categ_items,
             tolerance=1e-5)

})
