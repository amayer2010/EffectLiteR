

############# documentation ###############

#' EffectLiteR
#' 
#' Use structural equation modeling to estimate average and conditional effects 
#' of a treatment variable on an outcome variable, taking into account multiple
#' continuous and categorical covariates.
"_PACKAGE"


#' Dataset nonortho.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y. Continuous dependent variable depression.
#'   \item x. Treatment variable with values 0 (control), 1 (treat1), and 2 (treat2).
#'   \item z. Categorical covariate with values 0 (low neediness), 1 (medium neediness) and 2 (high neediness).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 500 rows and 3 variables
#' @name nonortho
NULL



#' Dataset example01.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item x. Treatment variable with values control, treat1, and treat2.
#'   \item k1. Categorical covariate with values male and female.
#'   \item kateg2. Categorical covariate with values 1 and 2.
#'   \item z1-z3. Continuous covariates.
#'   \item dv. Coninuous dependent variable.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 2000 rows and 7 variables.
#' @name example01
NULL



#' Dataset example02lv.
#' 
#' A simulated dataset with latent variables. The variables are:
#' 
#' \itemize{
#'   \item CPM11. First indicator of latent covariate.
#'   \item CPM21. Second indicator of latent covariate.
#'   \item CPM12. First indicator of latent outcome.
#'   \item CPM22. Second indicator of latent outcome.
#'   \item x. Dichotomous treatment variable with values 0 (control), and 1 (treatment).
#'   \item k. Categorical covariate with values first, second, and third.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 300 rows and 6 variables.
#' @name example02lv
NULL


#' Dataset MDRS2016.
#' 
#' The simulated dataset with latent variables used in Mayer, Dietzfelbinger, Rosseel, and Steyer (2016). The variables are:
#' 
#' \itemize{
#'   \item y11. First indicator of latent covariate (pretest mental health).
#'   \item y21. Second indicator of latent covariate (pretest mental health).
#'   \item y31. Third indicator of latent covariate (pretest mental health).
#'   \item y12. First indicator of latent outcome (posttest mental health).
#'   \item y22. Second indicator of latent outcome (posttest mental health).
#'   \item y32. Third indicator of latent outcome (posttest mental health).
#'   \item x. Categorical treatment variable with values 0 (wait list control group), 1 (conventional therapy), and 2 (innovative therapy).
#'   \item k. Categorical covariate with values 0 (male) and 1 (female).
#'   \item Ix1. Binary indicator for conventional therapy (X=1).
#'   \item Ix2. Binary indicator for innovative therapy (X=2).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 1000 rows and 10 variables.
#' @name MDRS2016
NULL


#' Dataset example_multilevel.
#' 
#' A simulated dataset with a cluster ID and sampling weights to test multilevel options. The variables are:
#' 
#' \itemize{
#'   \item y. Coninuous dependent variable.
#'   \item x. Treatment variable with values 0, 1.
#'   \item z. Continuous covariate.
#'   \item xz. Product of x and z.
#'   \item cid. Cluster ID.
#'   \item weights. Sampling weights.
#'   \item iptw. Classic inverse probability of treatment weights based on a logistic regression of x on z. Use with care (only for average effects).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 800 rows and 7 variables.
#' @name example_multilevel
NULL


#' Dataset sophonet_data_simulated.
#' 
#' A simulated dataset based on the SOPHONET-study (Leichsenring et al., 2013). The variables are:
#' 
#' \itemize{
#'   \item lsas.a.t2 
#'   \item lsas.v.t2 
#'   \item lsas.a.t1 
#'   \item lsas.v.t1 
#'   \item bdi.t1.i1 
#'   \item bdi.t1.i2 
#'   \item bdi.t1.i3 
#'   \item ecr.anx.t1.i1 
#'   \item ecr.anx.t1.i2 
#'   \item ecr.anx.t1.i3 
#'   \item ecr.avoi.t1.i1 
#'   \item ecr.avoi.t1.i2 
#'   \item ecr.avoi.t1.i3 
#'   \item tpq.ha.i1 
#'   \item tpq.ha.i2 
#'   \item tpq.ha.i3 
#'   \item tosca.shame.t1.i1 
#'   \item tosca.shame.t1.i2 
#'   \item fskn.se.t1.i1 
#'   \item fskn.se.t1.i2 
#'   \item comorbid 
#'   \item iip.lov 
#'   \item iip.dom tb
#' }
#' 
#' @docType data
#' @keywords datasets
#' @references Leichsenring, F., Salzer, S., Beutel, M. E., Herpertz, S., Hiller, W., Hoyer, J., Huesing, J., ..., Leibing, E. (2013). Psychodynamic therapy and cognitive-behavioral therapy in social anxiety disorder: A multicenter randomized controlled trial. American Journal of Psychiatry, 170, 759–767.
#' @format A data frame with 328 rows and 24 variables.
#' @name sophonet_data_simulated
NULL


#' Dataset elrdata_categorical_items.
#' 
#' A simulated dataset for testing measurement models with categorical items:
#' 
#' \itemize{
#'   \item x. Treatment variable with values 0, 1.
#'   \item z11. indicator for covariate.
#'   \item z21. indicator for covariate.
#'   \item z31. indicator for covariate.
#'   \item z41. indicator for covariate.
#'   \item z51. indicator for covariate.
#'   \item y11. indicator for outcome.
#'   \item y21. indicator for outcome.
#'   \item y31. indicator for outcome.
#'   \item y41. indicator for outcome.
#'   \item y51. indicator for outcome.
#'   \item y61. indicator for outcome.
#'   \item y71. indicator for outcome.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 10000 rows and 13 variables.
#' @name elrdata_categorical_items
NULL



#' Dataset elrdata_logreg.
#' 
#' A simulated dataset for testing logistic regression:
#' 
#' \itemize{
#'   \item y. Outcome variable with values 0, 1.
#'   \item x. Treatment variable with values 0, 1.
#'   \item z1. continuous covariate.
#'   \item z2. continuous covariate.
#'   \item k1. categorical covariate.
#'   \item k2. categorical covariate.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 10000 rows and 6 variables.
#' @name elrdata_logreg
NULL


#' Dataset elrdata_kieferetal2024.
#' 
#' A simulated dataset for logistic regression from Kiefer, Lugauer, and Mayer (2024):
#' 
#' \itemize{
#'   \item Y. Outcome variable with values 0, 1.
#'   \item X. Treatment variable with values 0, 1.
#'   \item Z. continuous covariate.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 600 rows and 3 variables.
#' @name elrdata_kieferetal2024
NULL



############## namespace ###########

#' @importFrom methods new is
NULL

#' @importMethodsFrom methods show 
NULL

#' @importFrom stats as.formula ftable model.frame model.matrix pnorm relevel var qnorm cov lm mahalanobis pchisq pf pt sd na.omit
NULL

#' @importFrom utils capture.output read.csv read.csv2 read.table combn
NULL

#' @importFrom numDeriv jacobian
NULL


