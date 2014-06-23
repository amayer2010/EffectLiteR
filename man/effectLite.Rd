\name{effectLite}
\alias{effectLite}
\title{Estimate average and conditional effects}
\usage{
effectLite(y, x, k = NULL, z = NULL, control = "0",
  measurement = character(), data, fixed.cell = FALSE,
  missing = "listwise", se = "standard", bootstrap = 1000L,
  syntax.only = FALSE, ...)
}
\arguments{
  \item{y}{Dependent variable (character string).}

  \item{x}{Treatment variable (character string).}

  \item{k}{Vector of categorical covariates (character
  vector).}

  \item{z}{Vector of continuous covariates (character
  vector).}

  \item{control}{Value of x that should be used as control
  group.}

  \item{measurement}{Measurement model.}

  \item{data}{A data frame.}

  \item{fixed.cell}{logical. If FALSE (default), the group
  sizes are treated as stochastic rather than fixed.}

  \item{missing}{Missing data handling to be used by
  lavaan::sem(). Will be passed on to sem()}

  \item{se}{Standard error to be used by lavaan::sem().
  Will be passed on to sem().}

  \item{bootstrap}{Number of bootstrap drwas, if
  bootstrapping is used. Will be passed on to
  lavaan::sem()-}

  \item{syntax.only}{logical. If TRUE, only syntax is
  returned and the model will not be estimated.}

  \item{...}{Further arguments passed to lavaan::sem().}
}
\value{
Object of class effectlite.
}
\description{
This function is the main funtion of the package and can be
used to estimate average and conditional effects of a
treatment variable on an outcome variable, taking into
account any number of continuous and categorical
covariates. It automatically generates lavaan syntax for a
multi-group structural equation model, runs the model in
lavaan, and extracts various average and conditional
effects of interest.
}
\examples{
m1 <- effectLite(y="y", x="x", k="z", control="0", data=nonortho)
print(m1)
}

