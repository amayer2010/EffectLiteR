

test_that("effectLite lm method works",{

  #### basic tests with method="lm" #####
  
  ## 1 K; 1 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1"), x="x", 
                   control="control", method="lm", interaction="all")
  
  ## 2 K; 1 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
                   control="control", method="lm")
  
  ## 2 K; 0 Z
  m1 <- effectLite(data=example01, y="dv", z=NULL, k=c("k1","kateg2"), x="x", 
                   method="lm", control="control")
  m2 <- effectLite(data=example01, y="dv", z=NULL, k=c("k1","kateg2"), x="x", fixed.cell=TRUE,
                   method="sem", control="control", homoscedasticity=TRUE)
  
  expect_equal(m1@results@Egxgk$Estimate,
               m2@results@Egxgk$Estimate,
               tolerance=1e-4) 
  
  ## 0 K; 0 Z
  m1 <- effectLite(data=example01, y="dv", method="lm", z=NULL, k=NULL, x="x", control="control")
  
  
  ## 0 K; 1 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1"), k=NULL, x="x", control="control",
                   method="lm")
  
  
  ## 2 K; 3 Z
  m1 <- effectLite(data=example01, y="dv", z=c("z1","z2","z3"), 
                   k=c("k1","kateg2"), x="x", control="control",
                   method="lm")
  
  m2 <- effectLite(data=example01, y="dv", z=c("z1","z2","z3"), 
                   k=c("k1","kateg2"), x="x", control="control",
                   method="sem", homoscedasticity=TRUE)
  
  expect_equal(m1@results@Egxgxk$Estimate,
               m2@results@Egxgxk$Estimate,
               tolerance=1e-5) 

})



test_that("comparison with Anova works",{
  
  ## compare with Anova (in the balanced case)
  current.contrast.action <- options('contrasts')
  on.exit(options(current.contrast.action))  
  
  require(car)
  
  set.seed(324)
  N <- 150
  design <- expand.grid(k=0:2, x=0:1)
  design$y <- c(40,30,70,50,20,80)
  ind <- rep(1:6, each=25)
  d <- design[ind,]
  d$y <- d$y + rnorm(N,0,5)
  d$x <- as.factor(d$x)
  d$k <- as.factor(d$k)
  
  options(contrasts=c("contr.treatment","contr.poly"))
  
  m1 <- effectLite(y="y", x="x", k="k", control="0", data=d, 
                   fixed.cell=TRUE, method="sem", homoscedasticity=TRUE)
  
  m2 <- effectLite(y="y", x="x", k="k", control="0", data=d, method="lm")
  
  options(contrasts=c("contr.sum","contr.poly"))
  m3 <- lm(y ~ x*k, data=d)
  
  
  tmp0 <- m1@results@hypotheses[[1,1]] ## Wald chisquare
  tmp1 <- m2@results@hypotheses[[1,1]] ## F value
  tmp2 <- Anova(m3, type="III")[2,3]
  
  expect_equal(tmp0, 10.15771, tolerance=1e-5)
  expect_equal(tmp1,tmp2) ## main effect x and p value
  
  tmp1 <- m2@results@hypotheses[[1,4]] ## p values
  tmp2 <- Anova(m3, type="III")[2,4]
  expect_equal(tmp1,tmp2)
  
  tmp1 <- m2@results@hypotheses[[3,1]] ## interaction term
  tmp2 <- Anova(m3, type="III")[4,3]
  expect_equal(tmp1,tmp2)
  
  tmp1 <- m2@results@hypotheses[[3,4]] ## p value interaction term
  tmp2 <- Anova(m3, type="III")[4,4]
  expect_equal(tmp1,tmp2)
  
})  

  

test_that("interaction argument works in lm",{
  
  #### tests with interaction option in lm ####
  
  m1 <- effectLite(y="dv", x="x", k="k1", data=example01,
                   fixed.cell=TRUE, method="sem", homoscedasticity=TRUE,
                   interactions="none")
  
  m2 <- effectLite(y="dv", x="x", k="k1", data=example01, method="lm",
                   interactions="none")
  
  expect_equal(m1@results@Egxgx[,1],  ## sem
               m2@results@Egxgx[,1],  ## lm
               tolerance=1e-5)
  
  expect_equal(head(m1@results@condeffects)[,c(4,6)],  ## sem
               head(m2@results@condeffects)[,c(4,6)],  ## lm
               tolerance=1e-4)
  
  
  m1 <- effectLite(y="dv", x="x", k="k1", data=example01, method="lm",
                   interactions="X:K")
  
  m1 <- effectLite(y="dv", x="x", k="k1", data=example01, method="lm",
                   interactions="X:Z")
  
  m1 <- effectLite(y="dv", x="x", k="k1", data=example01, method="lm",
                   interactions="none")
  
  m1 <- effectLite(y="dv", x="x", k="k1", data=example01, method="lm",
                   interactions="all")
  
})



test_that("stochastic group sizes work with lm",{
  
  ### first test with stochastic group sizes and lm
  m1 <- effectLite(data=nonortho, y="y", k="z", x="x", method="lm", fixed.cell=FALSE)
  
  expect_equal(m1@results@se[["Pk2"]], 0.019416282, tolerance=1e-5)
  
})
  
    