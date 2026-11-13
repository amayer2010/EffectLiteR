

test_that("elrEffects works",{
  
############ elrPredict tests ########################

### Without Z
m1.logistic.reg <- glm(Y ~ X, data=elrdata_kieferetal2024, family=binomial)  
m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ATE")
  
# numbers from Kiefer, Lugauer, & Mayer (2024)
m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ATE")
expected_estimate <- -0.038
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="SRA")
expected_estimate <- 0.930
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ORA")
expected_estimate <- 0.858
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

## With Z  
m1.logistic.reg <- glm(Y ~ X*Z, data=elrdata_kieferetal2024, family=binomial)  
m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ATE")

# numbers from Kiefer, Lugauer, & Mayer (2024)
m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ATE")
expected_estimate <- -0.033
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="SRA")
expected_estimate <- 0.940
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ORA")
expected_estimate <- 0.878
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="ASR")
expected_estimate <- 0.831
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)

m1 <- elrEffects(m1.logistic.reg, x="X", from=0, to=1, type="AOR")
expected_estimate <- 6.435
actual_estimate <- m1$ave
expect_equal(actual_estimate, expected_estimate, tolerance=1e-3)


## Using mean centering (average effects on the logit metric?), not in the paper, odds ratio at the mean of Z
m1.logistic.reg <- glm(Y ~ X*I(Z-mean(Z)), data=elrdata_kieferetal2024, family=binomial)  
expected_estimate <- 0.9479875
actual_estimate <- exp(m1.logistic.reg$coefficients[[2]])
expect_equal(actual_estimate, expected_estimate, tolerance=1e-5)


})
