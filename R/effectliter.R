


#' Estimate average and conditional effects
#' 
#' This function is the main function of the package and can be used to estimate
#' average and conditional effects of a treatment variable on an outcome variable,
#' taking into account any number of continuous and categorical covariates.
#' It automatically generates lavaan syntax for a multi-group structural equation
#' model, runs the model using lavaan, and extracts various average and conditional
#' effects of interest.
#' 
#' @param y Dependent variable (character string). Can be the name of a manifest variable or of a latent variable.
#' @param x Treatment variable (character string) treated as categorical variable.
#' @param k Vector of manifest variables treated as categorical covariates (character vector).
#' @param z Vector of continuous covariates (character vector). Names of both manifest and latent variables are allowed.
#' @param data A data frame.
#' @param method Can be one of \code{c("sem","lm")} and indicates which function is used to fit the model.
#' @param control Value of \code{x} that is used as control group. If "default", takes the first entry of \code{as.factor(x)}.
#' @param measurement Measurement model. The measurement model is lavaan syntax (character string), that will be appended before the automatically generated lavaan input. It can be used to specify a measurement for a latent outcome variable and/or latent covariates. See also the example and \code{\link[EffectLiteR]{generateMeasurementModel}}.
#' @param fixed.cell logical. If \code{FALSE}, the group sizes are treated as stochastic rather than fixed. The default setting for \code{method="sem"} is \code{FALSE} and the default setting for \code{method="lm"} is \code{TRUE}.
#' @param fixed.z logical. If \code{FALSE}, the continuous covariates are treated as stochastic rather than fixed. The default setting for \code{method="sem"} is \code{FALSE} and the default setting for \code{method="lm"} is \code{TRUE}. 
#' @param missing Missing data handling. Will be passed on to \code{\link[lavaan]{sem}} or ignored for \code{method="lm"}.
#' @param se Type of standard errors. Will be 
#' passed on to \code{\link[lavaan]{sem}} or ignored for \code{method="lm"}.
#' @param syntax.only logical. If \code{TRUE}, only syntax is returned and the model 
#' will not be estimated.
#' @param interactions character. Indicates the type of interaction. Can be one of \code{"all"} (all interactions), \code{"2-way"} (only two-way interactions), \code{"X:K,X:Z"} (only X:K and X:Z interactions), \code{"X:K"} (only X:K interactions), \code{"X:Z"} (only X:Z interactions), \code{"none"} (no treatment by covariate interactions, but potentially interactions between categorical and continuous covariates), or \code{"no"} (no interactions at all). 
#' @param homoscedasticity logical. If \code{TRUE}, residual variances of the dependent variable are assumed to be homogeneous across cells. The default setting for \code{method="sem"} is \code{FALSE} and the default setting for \code{method="lm"} is \code{TRUE}.
#' @param test.stat character. Can be one of \code{c("default","Chisq","Ftest")} and indicates the statistic used for the hypothesis tests. The tests are either based on the large sample Chi-Squared statistic (Wald tests) or the finite sample F statistic with approximate F distribution.  The default setting for \code{method="sem"} is \code{"Chisq"} and the default setting for \code{method="lm"} is \code{"Ftest"}.
#' @param propscore Vector of covariates (character vector) that will be used to compute (multiple) propensity scores based on a multinomial regression without interactions. Alternatively, the user can specify a formula with the treatment variable as dependent variable for more control over the propensity score model.
#' @param ids Formula specifying cluster ID variable. Because \code{lavaan.survey} that used this argument is no longer on CRAN, the \code{cluster} argument in \code{\link[lavaan]{sem}} will now be used. 
#' @param weights Formula to specify sampling weights. Because \code{lavaan.survey} that used this argument is no longer on CRAN, the \code{sampling.weights} argument in \code{\link[lavaan]{sem}} will now be used. Note: Only use weights if you know what you are doing. For example, some conditional treatment effects may require different weights than average effects.
#' @param add Character string that will be pasted at the end of the generated lavaan syntax. Can for example be used to add additional (in-) equality constraints or to compute user-defined conditional effects.
#' @param ... Further arguments passed to \code{\link[lavaan]{sem}}.
#' @return Object of class effectlite.
#' @references Mayer, A., Dietzfelbinger, L., Rosseel, Y. & Steyer, R. (2016). The EffectLiteR approach for analyzing average and conditional effects. Multivariate Behavioral Research, 51, 374-391.
#' @examples
#' ## Example with one categorical covariate
#' m1 <- effectLite(y="y", x="x", k="z", control="0", data=nonortho)
#' print(m1) 
#' 
#' ## Example with one categorical and one continuous covariate
#' m1 <- effectLite(y="dv", x="x", k=c("k1"), z=c("z1"), control="control", data=example01)
#' print(m1)
#' 
#' ## Example with latent outcome and latent covariate
#' measurement <- '
#' eta2 =~ 1*CPM12 + 1*CPM22
#' eta1 =~ 1*CPM11 + 1*CPM21
#' CPM11 + CPM12 ~ 0*1
#' CPM21 ~ c(m,m)*1
#' CPM22 ~ c(p,p)*1'
#'
#' m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
#'                  measurement=measurement, data=example02lv)
#' print(m1)
#' 
#' ## Example with cluster variable and sampling weights
#' m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
#'                     syntax.only=FALSE, data=example_multilevel, 
#'                     cluster="cid", sampling.weights="weights")
#' print(m1)
#' 
#' @export
#' @import lavaan
effectLite <- function(y, x, k=NULL, z=NULL, data, method="sem", control="default", 
                       measurement=character(), fixed.cell="default", 
                       fixed.z="default", missing="default", se="default", 
                       syntax.only=FALSE, interactions="all", homoscedasticity="default",
                       test.stat="default", propscore=NULL, ids=~0, weights=NULL, 
                       add=character(),...){
  
  obj <- new("effectlite")
  obj@call <- match.call()
  method_args <- list(...)
  obj@input <- createInput(y, x, k, z, data, method, control, measurement, 
                           fixed.cell, fixed.z, missing, se, interactions, 
                           homoscedasticity, test.stat, propscore, ids, weights,
                           add, method_args)
  obj@input <- computePropensityScore(obj@input)
  obj@parnames <- createParNames(obj)  
  obj@syntax <- createSyntax(obj)
  
  if(syntax.only){return(obj@syntax@model)}    
  obj@results <- computeResults(obj)
  obj@results@condeffects <- computeConditionalEffects(obj)
  
  return(obj)  
}




