#' Estimate linear regression models with an instrumental variable.
#'
#' This function allows you to estimate a two-stage least squares linear regression in one step.
#' @param model_formula The formula of the main regression problem.
#' @param instrument_formula The formula for the first stage of the regression problem.  Estimate an endogenous variable from one (or more) instruments.  If blank, defaults to ordinary linear regression.
#' @param data (optional) A data frame containing all variables for the regression model.
#' @param ... Additional arguments to pass to glm().
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
#' fit_ols <- lm(y ~ x)
#'
#' # Fit 2SLS
#' fit_2sls <- iv.lm(y ~ x, x ~ z)
#'
#' summary(fit_ols)
#' summary(fit_2sls)

iv.lm <- function(model_formula,
                 instrument_formula = NULL, data = NULL, ...) {
  if(is.null(instrument_formula)) {
    out <- lm(model_formula, data = data)
  } else {
    if(class(model_formula) != "formula" | class(instrument_formula) != "formula") {
      stop("both model_formula and instrument_formula must be of class formula")
    }
    # Instrument info
    instrument_data <- find_instruments(model_formula, instrument_formula)
    instruments <- instrument_data$instruments
    instrumented <- instrument_data$instrumented
    n_instruments <- length(instruments)
    if(length(instrumented) > 1) {
      stop("You may only instrument one variable at a time.")
    }
    # Estimate model
    stage_one <- lm(instrument_formula, data = model.frame(instrument_formula, data = data))
    model_data <- model.frame(model_formula, data = data)
    model_data[, which(names(model_data) == instrumented)] <- stage_one$fitted.values
    stage_two <- lm(formula = as.formula(deparse(model_formula)), data = model_data)

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
      instrumented = instrumented,
      stage_one = stage_one,
      fit = stage_two
    )
    class(out) <- "ivm"
  }
  return(out)
}
