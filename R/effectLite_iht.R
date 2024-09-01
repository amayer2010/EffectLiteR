# informative hypothesis testing for effectLite
#
# constraints= argument can be used to specify 'ordered' hypotheses:
# for example:
# constraints = 'Eg2 > Eg1; Eg1 > 0'
#
# for now, the ingredients must coincide with the internal
# effectLite variable names

# YR 05 Feb 2024: first version


#' Informative hypothesis tests for effectLite
#'
#' @param object effectlite. Fitted model of class effectlite estimated with \code{\link[EffectLiteR]{effectLite}} using \code{method="sem"}.
#' @param constraints character. Specification of constraints for the ordered hypothesis test.
#' @param test character. Statistical test to be used for the ordered hypothesis test. Can be one of \code{c("default", "Fbar", "Wald")}. 
#'
#' @return list with test statistics and p-value.
#' @export
#'
#' @examples
#' m1 <- effectLite(y="dv", x="x", k="k1", z="z1", method = "sem", 
#'                  fixed.cell=TRUE, fixed.z=TRUE, data=example01)
#' test <- effectLite_iht(object = m1, 
#'                        constraints = 'adjmean2 > adjmean1
#'                                       adjmean1 > adjmean0')
#' print(test)
#' 
#' @import restriktor
effectLite_iht <- function(object, constraints = NULL, test = "default") {

    if(!inherits(object, "effectlite")) {
        stop("object is not (or does not inherit from) class ",
              dQuote("effectLite"))
    }

    if(object@input@method != "sem") {
        stop("please rerun effectLite with method = ", dQuote("sem"))
    }

    if(is.null(constraints)) {
        stop("please use the constraints= argument to specify the ordered hypothesis")
    }
  
  stopifnot(test %in% c("default", "Fbar", "Wald"))
    
    # lavaan object (without constraints)
    lavfit.unco <- object@results@lavresults
    Info.unco.inv <- lavTech(lavfit.unco, "inverted.information")

    # parse constraints
    CON <- lav_constraints_parse(partable = lavfit.unco@ParTable,
                                 constraints = constraints)

    # evaluate constraints
    con.values <- CON$cin.function(coef(lavfit.unco))
    if(all(con.values >= 0)) {
        # good! constraints are satisfied! no need to fit a 'constrained' model
        lavfit.final <- lavfit.unco; rm(lavfit.unco)
    } else {
        # refit, imposing constraints
        lavfit.con <- update(lavfit.unco, constraints = constraints)
        lavfit.final <- lavfit.con; rm(lavfit.con)
    }

    # compute Wald_info statistic
    ntotal <- lavTech(lavfit.final, "ntotal")
    R.H1 <- CON$cin.JAC
    gamma.H1 <- coef(lavfit.final)
    Wald.info.A <- ntotal * drop( t(R.H1 %*% gamma.H1) %*%
                                  solve(R.H1 %*% Info.unco.inv %*% t(R.H1)) %*%
                                  R.H1 %*% gamma.H1 )

    # calculate p-value
    meq <- 0 # for now
    if(meq == 0L) { 
        wt <- ic.infer::ic.weights(R.H1 %*% Info.unco.inv %*% t(R.H1))
    } # else if(meq > 0) {
      #  tmp <- solve(R.H1 %*% VCOV.X %*% t(R.H1))[-(1:meq), -(1:meq)]
      #  wt <- ic.infer::ic.weights(solve(tmp))
    # }
    wt.rev <- rev(wt)

    # df1
    df1 <- 0:(nrow(R.H1) - meq)
    
    # df2?
    if(test == "default"){
      ## method="lm" is currently not allowed but will then be tested here
      linear.lm.flag <- all(lavfit.final@Model@modprop$uvreg)
      if(linear.lm.flag){
        test.stat <- "Fbar"
      }else{
        test.stat <- "Wald"
      }
      
    }else if(tolower(test) == "fbar"){
      test.stat <- "Fbar"
      
    }else if(tolower(test) == "wald"){
      test.stat <- "Wald"
    }
    
    if(test.stat == "Fbar") {
        df2 <- ntotal - length(object@parnames@unconstrainedgammas)
        pvalue <- ( 1 - pfbar(Wald.info.A, 
                                           df1 = rev(df1), 
                                           df2 = df2, 
                                           wt.bar = wt) )
    } else {
        pvalue <- ( 1 - pfbar(Wald.info.A, 
                              df1 = rev(df1), 
                              df2 = +Inf,
                              wt.bar = wt) )
    }

    # output list
    out <- list(test.stat = test.stat, 
                Wald.info = Wald.info.A, 
                pvalue = pvalue)

    out
}



## Note YR: The pfbar() function is taken from restriktor
pfbar <- function(x, df1, df2, wt.bar) {
  if (x <= 0) {
    return(0)
  }
  zed <- df1 == 0
  cdf <- ifelse(any(zed), wt.bar[zed], 0)
  cdf <- cdf + sum(pf(x/df1[!zed], df1[!zed], df2) * wt.bar[!zed])
  return(cdf)
}
