

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

expected_effects_categ_items <- data.frame(
  x1 = c(0.11067810, 0.27555281, 0.02873438),
  x2 = c(0.10191910, 0.08317492, 0.12862313),
  x3 = c(1.0859408, 3.3129316, 0.2233998),
  x4 = c(0.2775052007, 0.0009232356, 0.8232243450),
  x5 = c(0.1645116, 0.4095809, 0.0427107)
)

names(expected_effects_categ_items) <- c("Estimate", "SE", "Est./SE", "p-value",
                                         "Effect Size")
row.names(expected_effects_categ_items) <- c("Eg1", "Eg1gx0", "Eg1gx1")

expect_equal(actual_effects_categ_items,
             expected_effects_categ_items,
             tolerance=1e-5)

})
