######################## class definitions #####################

## structure of class effectlite
# - call
# - user input (class input)
# - parameter names (class parnames)
# - generated syntax (class syntax)
# - obtained results (class results)

setClass("input", representation(
  method="character", ## sem or lm to fit object
  vnames="list", ## variable names
  vlevels="list", ## variable levels (for x, k, kstar and cell)
  control="character",
  ng="integer", ## number of treatment groups
  nz="integer", ## number of z
  nk="integer", ## number of unfolded categories of K  
  data="data.frame", 
  measurement="character",
  add="character",
  fixed.cell ="logical",
  fixed.z ="logical",
  missing="character",
  observed.freq="numeric", ## observed group frequencies (fixed.cell only)
  sampmeanz="array", ## manifest sample means for continuous covriates
  se="character", ## lavaan standard errors
  interactions="character", ## type of interaction (all, 2-way, no)
  complexsurvey="list",
  homoscedasticity="logical",
  test.stat="character",
  outprop="list", ## output from propensity score model
  method_args="list" ## additional ... arguments passed to method (sem, lm)
)
)

setClass("parnames", representation(
  alphas="array", 
  betas="array", 
  gammas="array",
  constrainedgammas="character",
  unconstrainedgammas="character",
  gammalabels="array",
  label.g.function="character",
  label.covs="character",
  label.Egx="character",
  cellmeanz="character",
  meanz="character",
  pk="character",
  px="character",
  Ezk="character",
  Pkgx="character", ## P(K=k|X=x)
  Pxgk="character", ## P(X=x|K=k)
  Ezgx="character", ## E(Z|X=x)
  Ezgk="character", ## E(Z|K=k)
  Ezkgx="character", ## E(Z*K|X=x)
  groupw="character",
  relfreq="character",
  Egx="character",
  Egxgx="character", ## E(gx|X=x)
  Egxgk="character", ## E(gx|K=k)
  Egxgxk="character", ## E(gx|X=x,K=k)
  adjmeans="character",
  adjmeansgk="character",
  AveEffZ="character" ## average effect continuous covariate Z
)
)

setClass("syntax", representation(
  model="character", 
  hypotheses="list",
  hypothesesk="list"
)
)


setClass("results", representation(
  lavresults="lavaan",
  lmresults="lm",
  est="numeric",
  se="numeric",
  vcov.def="matrix",
  hypotheses="data.frame",
  hypothesesk="data.frame",
  Egx="data.frame",
  AdditionalEffects="data.frame",
  Egxgx="data.frame",
  Egxgk="data.frame",
  Egxgxk="data.frame",
  gx="list",
  adjmeans="data.frame",
  adjmeansgk="data.frame",
  AveEffZ="data.frame",
  condeffects="data.frame"
)
)


setClass("effectlite", representation(
  call="call",
  input="input",
  parnames="parnames",
  syntax="syntax",
  results="results"
)
)
