#' Estimate linear regression models with an instrumental variable.
#'
#' This function allows you to estimate a two-stage least squares linear regression in one step.
#' @param formula the formula of the linear regression
#' @param instrument_formula
#' @data
#' @keywords instrument
#' @export
#' @examples
#' # Fake data
#' N <- 1000
#' z <- rnorm(N, 1, 1)
#' error <- rnorm(N, 1, 1)
#' x <- z + error + rnorm(N, 1, 1)
#' y <- x + error
#'
#' # Fit OLS
#' fit_ols <- lm(formula = y ~ x)
#'
#' # Fit 2SLS
#' fit_2sls <- iv.lm(formula = y ~ instrument, instrument_formula = x ~ z)
#'
#' summary(fit_ols)
#' summary(fit_2sls)

iv.lm <- function(formula,
                 instrument_formula, data = NULL, ...) {
  if(class(formula) != "formula" | class(instrument_formula) != "formula") {
    stop("both formula and instrument_formula must be of class formula")
  }
  if(length(grep("instrument", formula)) == 0) {
    stop("you must include instrument in the formula as a placeholder")
  }
  if(length(instrument_formula) != 3) {
    stop("instrument_formula must be a formula with one dependent and one independent variable")
  }

  # Instrument info
  instruments <- all.vars(instrument_formula)[-1]
  n_instruments <- length(instruments)

  # Estimate model
  stage_one <- lm(instrument_formula, data = data)
  assign(paste0('instrument'), stage_one$fitted.values,  envir = environment())
  stage_two <- lm(formula = as.formula(deparse(formula)), data = data)

  # Diagnostics
  cor_w_var <- vector('numeric', length = n_instruments)
  cor_w_error <- vector('numeric', length = n_instruments)
  for(i in seq_along(1:n_instruments)) {
    cor_w_var[i] <- cor(stage_one$model[, 1], stage_one$model[, (i+1)])
    cor_w_error[i] <- cor(stage_one$model[, (i+1)], stage_two$residuals)
    cat(paste0("correlation between ", names(stage_one$model)[1], " and ", names(stage_one$model)[(i+1)], ": ",
               round(cor_w_var[i], 3),  "\n"))
    cat(paste0("correlation between ", names(stage_one$model)[(i+1)], " and residuals: ",
               round(cor_w_error[i], 3),  "\n"))
  }

  # Return results and diagnostics
  out <- list(
    exclusion_restriction = round(cor_w_error, 3),
    instrument_validity = round(cor_w_var, 3),
    instruments = instruments,
    stage_one = stage_one,
    fit = stage_two
  )
  class(out) <- "ivm"
  return(out)
}


summary.ivm <- function(x) summary(x$fit)

print.ivm <- function(x) print(x$fit)

plot.ivm <- function(x) plot(x$fit)
