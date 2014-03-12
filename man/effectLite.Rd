\name{effectLite}
\alias{effectLite}
\title{Estimate average and conditional effects}
\usage{
effectLite(y, x, k = NULL, z = NULL, control = "0",
  measurement = character(), data, fixed.cell = FALSE,
  missing = "listwise", syntax.only = FALSE, ...)
}
\arguments{
  \item{y}{dependent variable (character string).}

  \item{x}{treatment variable (character string).}

  \item{k}{vector of categorical covariates (character
  vector).}

  \item{z}{vector of continuous covariates (character
  vector).}

  \item{measurement}{measurement model.}

  \item{data}{a data frame.}

  \item{fixed.cell}{logical. If FALSE (default), the group
  sizes are treated as stochastic rather than fixed.}

  \item{missing}{missing data handling to be used by
  lavaan::sem(). Will be passed on to sem()}

  \item{syntax.only}{logical. If TRUE, only syntax is
  returned and the model will not be estimated.}

  \item{...}{further arguments passed to lavaan::sem().}
}
\value{
object of class ELLavaanModel.
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
m1 <- effectLite(y="y", x="x", z="z2", k="z1", control="0", data=daten1411)
print(m1)
}

