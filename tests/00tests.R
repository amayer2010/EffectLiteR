
require(EffectLiteR)
require(testthat)
require(car)

## run test_local() to test all

test_file("tests/testthat/tests_effectlite_adjmean.R")
test_file("tests/testthat/tests_effectlite_basic.R")
test_file("tests/testthat/tests_effectlite_aggregated_effects.R")
test_file("tests/testthat/tests_effectlite_lm_method.R")
test_file("tests/testthat/tests_effectlite_latent_variables.R")
test_file("tests/testthat/tests_effectlite_complex_survey.R")
test_file("tests/testthat/tests_elrpredict.R")
test_file("tests/testthat/tests_effectlite_propscores.R")
test_file("tests/testthat/tests_effectlite_some_options.R")
test_file("tests/testthat/tests_effectlite_hypotheses.R")
test_file("tests/testthat/tests_effectlite_categorical_items.R")
test_file("tests/testthat/tests_effectlite_many_cells.R") ## takes some time...


## see also (does not work from here, because data won't be found)
# test_file("private/tests/tests_effectlite_with_private_data.R")


