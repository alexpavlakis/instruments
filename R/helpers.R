#' Diagnose problems with iv models.
#'
#' For instrumental variable models to be valid, instruments should be correlated with the variable for which they are instrumenting; and not correlated with the error term in the regression model.  This function diagnoses potential problems with either of those criteria.
#' @param x an object of class ivm
#' @param thresh arbitrary threshold for correlations.  defaults to 0.1.
#' @data
#' @keywords instrument
#' @export
#' @examples
#' # Fake data
#' N <- 1000
#' z <- rnorm(N, 1, 1)
#' q <- rnorm(N, 0, 1)
#' error <- rnorm(N, 1, 1)
#' x <- z + error + rnorm(N, 1, 1)
#' y <- x + error
#'
#' # Fit OLS
#' fit_ols <- lm(formula = y ~ x)
#'
#' # Fit 2SLS
#' fit_2sls <- iv.lm(formula = y ~ instrument, instrument_formula = x ~ z + q)
#'
#' diagnose(fit_2sls)

#' @export
diagnose <- function(x, thresh = 0.1) {
  if(class(x) != "ivm") {
    stop("diagnose() is for objects of class ivm")
  }
  n_instruments <- length(x$instruments)
  for(i in seq_along(1:n_instruments)) {
    if(abs(x$exclusion_restriction[i]) > thresh) {
      cat(paste0(x$exclusion_restriction[i], " correlation between ", x$instruments[i], " and residual. Exclusion restriction may not be satisfied \n"))
    }
    if(abs(x$instrument_validity[i]) < thresh) {
      cat(paste0(x$instrument_validity[i], " correlation between ", x$instruments[i], " and ", names(x$stage_one$model)[1] ,". ", x$instruments[i], " may not be a valid instrument \n \n"))
    }
  }
}

#' @export
invlogit <- function(x) 1/(1 + exp(-x))
