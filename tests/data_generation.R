

### generation of some of the datasets used for testing and also included in 
### the package


####################### complex example test ##################
# 
# set.seed(664488)
# 
# N <- 2000
# d <- expand.grid(x=c("control","treat1","treat2"),k1=c("male","female"), 
#                  kateg2=1:2)
# ind <- sample(1:nrow(d), size=N, replace=T)
# d <- d[ind,]
# d$z1 <- rnorm(N)
# d$z2 <- rnorm(N)
# d$z3 <- rnorm(N)
# d$dv <- rnorm(N)


######################### Example Daten1411.sav #####################
# 
# d <- daten1411
# test <- effectLite(y="y", x="x", z=c("Iz1g1","Iz1g2","z2"), control="0", data=d)
# 
# ## per Hand
# daten <- test@input@data
# model <- test@lavaansyntax@model
# 
# m1 <- sem(model, group="cell", group.label=levels(daten$cell), 
#           data=daten, fixed.x=F, group.w.free = TRUE, mimic="mplus", 
#           missing="ml")
# 
# test <- effectLite(y="y", x="x", z="z2", k="z1", control="0", data=d)



############ Example 01 with latent variable ################## 
# 
# set.seed(6636363)
# 
# N <- 300
# eta1 <- rnorm(N,2,2)
# x <- rbinom(N,1,plogis(eta1,location=2))
# k <- sample(c("first","second","third"), size=N, replace=T)
# eta2 <- 0.2 + 0.7*eta1 + 1*x + 0.5*x*eta1 + rnorm(N,0,0.3)
# 
# CPM11 <- eta1 + rnorm(N,0,0.4)
# CPM21 <- 0.5 + eta1 + rnorm(N,0,0.4)
# CPM12 <- eta2 + rnorm(N,0,0.4)
# CPM22 <- 0.5 + eta2 + rnorm(N,0,0.4)
# 
# d <- data.frame(CPM11, CPM21, CPM12, CPM22, x, k)
# example02lv <- d
# save(example02lv, file="data/example02lv.RData")



########## complex survey design #################
# 
# set.seed(2356)
# 
# N <- 800
# Nc <- 40
# csize <- N/Nc
# cid <- rep(1:Nc, each=csize)
# 
# ## treatment
# x <- rep(c(0,1), each=Nc/2)[cid]
# 
# ## covariate
# ujz <- rnorm(Nc, 0, sqrt(0.1))[cid]
# rij <- rnorm(N, 0, sqrt(1.5))
# z <- 0.4 + 0.7*x + ujz + rij
# xz <- x*z ## interaction
# 
# ## outcome
# ujy <- rnorm(Nc, 0, sqrt(0.2))[cid]
# eij <- rnorm(N, 0, sqrt(2))
# y <- 0 + 1*x + 0.6*z + 0.4*xz + ujy + eij
# weights <- plogis(rnorm(N)) ## random weights
# 
# ## inverse probability of treatment weighting (IPTW)
# propmodel <- glm(x ~ z, family=binomial)
# propensity <- predict(propmodel, type="response")
# iptw <- numeric(N)
# iptw[x==0] <- 1/(1-propensity)[x==0]
# iptw[x==1]<- 1/propensity[x==1]
# 
# example_multilevel <- data.frame(y, z, x, xz, cid, weights, iptw)
# 
# save(example_multilevel, file="example_multilevel.RData")
