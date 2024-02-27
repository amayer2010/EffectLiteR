

test_that("effectLite adjmeans 2k1z works",{

  ## adjmeans 2 K; 1 Z
  m1_2k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"),
                        x="x", control="control")
  
  actual_adjmeans_2k1z <- m1_2k1z@results@adjmeans
  
  expected_adjmeans_2k1z <- data.frame(
    Estimate=c(0.025841665, 0.005330852, 0.042114062),
    SE=c(0.03694562, 0.04047797, 0.03860001),
    Est.SE=c(0.6994514, 0.1316976, 1.0910376)
  )
  
  names(expected_adjmeans_2k1z)[3] <- "Est./SE"
  row.names(expected_adjmeans_2k1z) <- c("adjmean0", "adjmean1", "adjmean2")
  
  expect_equal(actual_adjmeans_2k1z, 
              expected_adjmeans_2k1z, 
              tolerance=1e-4)
  
  expect_equal(nrow(m1_2k1z@results@adjmeans), 3)

})



test_that("effectLite adjmeans 0k0z works",{
  
  ## 0 K; 0 Z
  m1_0k0z <- effectLite(data=example01, y="dv", z=NULL, k=NULL, x="x",
                        control="control")
  
  actual_adjmeans_0k0z <- m1_0k0z@results@adjmeans$Estimate
  expected_adjmeans_0k0z <- c(0.018218020, 0.008307386, 0.050080302)
  
  expect_equal(actual_adjmeans_0k0z, 
               expected_adjmeans_0k0z, 
               tolerance=1e-4)
  
  expect_equal(nrow(m1_0k0z@results@adjmeans), 3)
  expect_equal(ncol(m1_0k0z@results@adjmeans), 3)
  
})



test_that("effectLite adjmeans 0k1z works",{
  
  ## 0 K; 1 Z
  m1_0k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=NULL, x="x",
                        control="control")
  
  actual_adjmeans_0k1z <- m1_0k1z@results@adjmeans$`Est./SE`
  expected_adjmeans_0k1z <- c(0.5145544, 0.1998840, 1.3216327)
  
  expect_equal(actual_adjmeans_0k1z, 
               expected_adjmeans_0k1z, 
               tolerance=1e-4)

  expect_equal(nrow(m1_0k1z@results@adjmeans), 3)
  expect_equal(ncol(m1_0k1z@results@adjmeans), 3)
  
})



test_that("effectLite adjmeansxgk works",{

  ## adjmeans 2 K; 1 Z
  m1_2k1z <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"),
                        x="x", control="control")

  actual_adjmeansgk_2k1z <- m1_2k1z@results@adjmeansgk[c(1,5,7),]

  expected_adjmeansgk_2k1z <- data.frame(
    Estimate=c(0.06383352, -0.11255929, 0.08737369),
    SE=c(0.07196349, 0.08080879, 0.07053801),
    Est.SE=c(0.8870265, -1.3929090, 1.2386753)
  )

  names(expected_adjmeansgk_2k1z)[3] <- "Est./SE"
  row.names(expected_adjmeansgk_2k1z) <- c("adjmean0gk0", "adjmean1gk1", "adjmean0gk2")

  expect_equal(actual_adjmeansgk_2k1z,
               expected_adjmeansgk_2k1z,
               tolerance=1e-4)

  expect_equal(nrow(m1_2k1z@results@adjmeansgk), 12)
  
  actual_adjmeansgk_2k1z <- m1_2k1z@results@adjmeansgk
  actual_Egxgk_2k1z <- m1_2k1z@results@Egxgk
  
  expect_equal(actual_Egxgk_2k1z[1,1], 
               actual_adjmeansgk_2k1z[2,1] - actual_adjmeansgk_2k1z[1,1],
               tolerance=1e-6)
  
  expect_equal(actual_Egxgk_2k1z[4,1], 
               actual_adjmeansgk_2k1z[6,1] - actual_adjmeansgk_2k1z[4,1],
               tolerance=1e-6)
  
  expect_equal(actual_Egxgk_2k1z[8,1], 
               actual_adjmeansgk_2k1z[12,1] - actual_adjmeansgk_2k1z[10,1],
               tolerance=1e-6)
  
})


