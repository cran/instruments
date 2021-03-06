% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{diagnose}
\alias{diagnose}
\title{Diagnose problems with iv models.}
\usage{
diagnose(x, thresh = 0.1)
}
\arguments{
\item{x}{an object of class ivm}

\item{thresh}{arbitrary threshold for correlations.  defaults to 0.1.}
}
\description{
For instrumental variable models to be valid, instruments should be correlated with the variable for which they are instrumenting; and not correlated with the error term in the regression model.  This function diagnoses potential problems with either of those criteria.
}
\examples{
# Fake data
N <- 1000
z <- rnorm(N, 1, 1)
q <- rnorm(N, 0, 1)
error <- rnorm(N, 1, 1)
x <- z + error + rnorm(N, 1, 1)
y <- x + error

# Fit OLS
fit_ols <- lm(formula = y ~ x)

# Fit 2SLS
fit_2sls <- iv.lm(y ~ x, x ~ z + q)

diagnose(fit_2sls)
}
\keyword{instrument}
