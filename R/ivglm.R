#' Estimate linear regression models with an instrumental variable.
#'
#' This function allows you to estimate a two-stage least squares linear regression in one step.
#' @param formula The formula of the main regression problem.  Use `instrument` for the variable that is the result of the `instrument_formula` function.
#' @param instrument_formula The formula for the first stage of the regression problem.  Estimate an endogenous variable from one (or more) instruments.
#' @family The distribution family of the outcome variable.
#' @link The link function to the outcome variable.
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
#' Fit glm
#' fit_glm <- glm(formula = y ~ x, family = binomial(link = 'logit'))
#'
#' Fit with iv
#' fit_2sls <- iv.glm(formula = y ~ instrument, instrument_formula = x ~ z, family = binomial, link = 'logit')
#'
#' summary(fit_glm)
#' summary(fit_2sls)

iv.glm <- function(formula,
                   instrument_formula, data = NULL,
                   family = binomial, link = 'logit', ...) {
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
  stage_two <- glm(formula = as.formula(deparse(formula)), data = data,
                   family = family(link = link))

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
    stage_one = stage_one,
    fit = stage_two,
    instruments = instruments
  )
  class(out) <- "ivm"
  return(out)
}
