

test_that("effectLite 1k1z works",{

  ## 1 K; 1 Z
  m1_1k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x",
                        control="control")

  actual_effects_1k1z <- rbind(m1_1k1z@results@Egx,
                               m1_1k1z@results@Egxgx,
                               m1_1k1z@results@Egxgk,
                               m1_1k1z@results@Egxgxk)
  
  actual_effects_1k1z_est <- actual_effects_1k1z$Estimate
  actual_effects_1k1z_se <- actual_effects_1k1z$SE
  actual_effects_1k1z_zval <- actual_effects_1k1z$`Est./SE`
  actual_effects_1k1z_pval <- actual_effects_1k1z$`p-value`
  actual_effects_1k1z_es <- actual_effects_1k1z$`Effect Size`
  
  expected_effects_1k1z_est <- c(-0.01545, 0.02287, -0.00081, 0.0299, -0.01977, 
      0.0184, -0.02603, 0.01989, -0.15273, 0.01764, 0.1246, 0.02819, -0.13679, 
      0.03039, -0.16054, 0.01141, -0.16061, 0.01135, 0.12747, 0.02944, 0.11783, 
      0.02524, 0.12795, 0.02965)
  
  expected_effects_1k1z_se <- c(0.05495, 0.05333, 0.05528, 0.05353, 0.05536, 
      0.05374, 0.05562, 0.05347, 0.07901, 0.07387, 0.07606, 0.07699, 0.07953, 
      0.07422, 0.07957, 0.07434, 0.07947, 0.07427, 0.07626, 0.07695, 0.07625, 
      0.07756, 0.07629, 0.07695)
  
  expected_effects_1k1z_zval <- c(-0.28124, 0.42874, -0.0146, 0.5586, -0.35708, 
      0.34243, -0.46799, 0.37192, -1.93296, 0.23884, 1.63817, 0.36617, -1.71992, 
      0.40948, -2.01765, 0.15344, -2.0209, 0.15281, 1.67155, 0.38265, 1.5454, 
      0.32549, 1.67714, 0.38535)

  expected_effects_1k1z_pval <- c(0.77853, 0.66811, 0.98835, 0.57644, 0.72103, 
      0.73203, 0.63979, 0.70995, 0.05324, 0.81123, 0.10139, 0.71424, 0.08545, 
      0.68219, 0.04363, 0.87805, 0.04329, 0.87855, 0.09461, 0.70198, 0.12225, 
      0.74481, 0.09352, 0.69998)
  
  expected_effects_1k1z_es <- c(-0.01603, 0.02372, -0.00084, 0.03102, -0.02051, 
      0.01909, -0.02701, 0.02063, -0.15845, 0.0183, 0.12926, 0.02925, -0.1419, 
      0.03153, -0.16654, 0.01183, -0.16662, 0.01177, 0.13224, 0.03055, 0.12224, 
      0.02619, 0.13273, 0.03076)

  expect_equal(actual_effects_1k1z_est, 
               expected_effects_1k1z_est, 
               tolerance=1e-5)
  
  expect_equal(actual_effects_1k1z_se, 
               expected_effects_1k1z_se, 
               tolerance=1e-5)
  
  expect_equal(actual_effects_1k1z_zval, 
               expected_effects_1k1z_zval, 
               tolerance=1e-5)
  
  expect_equal(actual_effects_1k1z_pval, 
               expected_effects_1k1z_pval, 
               tolerance=1e-5)
  
  expect_equal(actual_effects_1k1z_es, 
               expected_effects_1k1z_es, 
               tolerance=1e-5)
  
})




test_that("effectLite 2k1z works",{
  
  ## 2 K; 1 Z
  m1_2k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), 
                        x="x", control="control")
  
  actual_effects_2k1z <- rbind(m1_2k1z@results@Egx,
                               m1_2k1z@results@Egxgx,
                               m1_2k1z@results@Egxgk,
                               m1_2k1z@results@Egxgxk)
  
  actual_effects_2k1z_zval <- actual_effects_2k1z$`Est./SE`

  expected_effects_2k1z_zval <- c(-0.3734, 0.30446, -0.05415, 0.37687, -0.32767, 
      0.19182, -0.69965, 0.32174, -1.03936, 0.59716, -1.82647, -0.59297, -0.4472, 
      -1.25516, 2.83455, 2.01397, -1.03835, 0.56211, -1.03359, 0.47871, -1.03888, 
      0.70377, -1.16289, -0.40261, -1.84562, -0.60749, -2.27681, -0.73591, 
      -0.48583, -1.2437, -0.50435, -1.23766, -0.34115, -1.27974, 2.76717, 
      2.12215, 2.87508, 1.83381, 2.82304, 2.03407)
  

  expect_equal(actual_effects_2k1z_zval, 
               expected_effects_2k1z_zval, 
               tolerance=1e-5)

})





test_that("effectLite 2k0z works",{
  
  ## 2 K; 0 Z
  m1_2k0z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), 
                        x="x", control="control")
  
  actual_effects_2k0z <- rbind(m1_2k0z@results@Egx,
                               m1_2k0z@results@Egxgx,
                               m1_2k0z@results@Egxgk,
                               m1_2k0z@results@Egxgxk)
  
  actual_effects_2k0z_zval <- actual_effects_2k0z$`Est./SE`
  
  expected_effects_2k0z_zval <- c(-0.3734, 0.30446, -0.05415, 0.37687, -0.32767, 
      0.19182, -0.69965, 0.32174, -1.03936, 0.59716, -1.82647, -0.59297, -0.4472, 
      -1.25516, 2.83455, 2.01397, -1.03835, 0.56211, -1.03359, 0.47871, -1.03888, 
      0.70377, -1.16289, -0.40261, -1.84562, -0.60749, -2.27681, -0.73591, 
      -0.48583, -1.2437, -0.50435, -1.23766, -0.34115, -1.27974, 2.76717, 
      2.12215, 2.87508, 1.83381, 2.82304, 2.03407)
  
  
  expect_equal(actual_effects_2k0z_zval, 
               expected_effects_2k0z_zval, 
               tolerance=1e-5)
  
})




test_that("effectLite 0k0z works",{
  
  ## 0 K; 0 Z
  m1_0k0z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), 
                        x="x", control="control")
  
  actual_effects_0k0z <- rbind(m1_0k0z@results@Egx,
                               m1_0k0z@results@Egxgx,
                               m1_0k0z@results@Egxgk,
                               m1_0k0z@results@Egxgxk)
  
  actual_effects_0k0z_zval <- actual_effects_0k0z$`Est./SE`
  
  expected_effects_0k0z_zval <- c(-0.3734, 0.30446, -0.05415, 0.37687, -0.32767, 
      0.19182, -0.69965, 0.32174, -1.03936, 0.59716, -1.82647, -0.59297, -0.4472, 
      -1.25516, 2.83455, 2.01397, -1.03835, 0.56211, -1.03359, 0.47871, -1.03888, 
      0.70377, -1.16289, -0.40261, -1.84562, -0.60749, -2.27681, -0.73591, 
      -0.48583, -1.2437, -0.50435, -1.23766, -0.34115, -1.27974, 2.76717, 
      2.12215, 2.87508, 1.83381, 2.82304, 2.03407)
  
  expect_equal(actual_effects_0k0z_zval, 
               expected_effects_0k0z_zval, 
               tolerance=1e-5)
  
})




test_that("effectLite 0k1z works",{
  
  ## 0 K; 1 Z
  m1_0k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=NULL, x="x", 
                        control="control")

  actual_averageeffect_0k1z_zval <- m1_0k1z@results@Egx[1,3]
  expected_averageeffect_0k1z_zval <- -0.1970662

  expect_equal(actual_averageeffect_0k1z_zval, 
               expected_averageeffect_0k1z_zval, 
               tolerance=1e-5)
  
})




test_that("effectLite 1k0z with interaction argument works",{
  
  ## 1 K 0 Z interaction argument
  m1_1k0z_int <- effectLite(data=example01, y="dv", k=c("k1"), x="x", 
                            control="control", interactions="X:Z")

  actual_averageeffect_1k0z_int <- m1_1k0z_int@results@Egx[1,3]
  expected_averageeffect_1k0z_int_zval <- -0.125133
  
  expect_equal(actual_averageeffect_1k0z_int, 
               expected_averageeffect_1k0z_int_zval, 
               tolerance=1e-5)
  
})



test_that("effectLite 1k0z works",{
  
  ## 1 K; 0 Z
  m1 <- effectLite(data=example01, y="dv", z=NULL, k="k1", x="x", 
                   control="control")

  expect_equal(nrow(m1@results@Egxgk), 4)
  
})



test_that("effectLite 2k2z works",{
  
  ## 2 K; 2 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1","z2"), 
                   k=c("k1","kateg2"), x="x", control="control")
  
  expect_equal(nrow(m1@results@Egxgk), 8)
  
})



test_that("effectLite 2k3z works",{
  
  ## 2 K; 3 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1","z2","z3"), 
                   k=c("k1","kateg2"), x="x", control="control")
  
  expect_equal(m1@results@Egxgk[7,5], 0.31738629, tolerance=1e-5)
  
})


test_that("effectLite 2k4z works",{
  
  ## 1 K; 4 Z
  dtmp <- example01
  dtmp$k1num <- as.numeric(dtmp$k1)
  m1 <- effectLite(data=dtmp, y="dv", z=c("z1","z2","z3","k1num"), 
                   k="kateg2", x="x", control="control")
  
  expect_equal(m1@results@Egxgk[1,4], 0.2578500, tolerance=1e-5)
  
})



