#' Estimate linear regression models with an instrumental variable.
#'
#' This function allows you to estimate a two-stage least squares linear regression in one step.
#' @param model_formula The formula of the main regression problem.  Use `instrument` for the variable that is the result of the `instrument_formula` function.
#' @param instrument_formula The formula for the first stage of the regression problem.  Estimate an endogenous variable from one (or more) instruments. If blank, defaults to regular glm.
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
#' fit_glm <- glm( y ~ x, family = binomial(link = 'logit'))
#'
#' Fit with iv
#' fit_iv <- iv.glm(y ~ x, x ~ z, family = binomial, link = 'logit')
#'
#' summary(fit_glm)
#' summary(fit_iv)

iv.glm <- function(model_formula,
                   instrument_formula = NULL, data = NULL,
                   family = binomial, link = 'logit', ...) {
  if(is.null(instrument_formula)) {
    out <- glm(model_formula, data = data, family = family(link = link))
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
    stage_two <- glm(formula = model_formula, data = model_data,
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
      instruments = instruments,
      instrumented = instrumented,
      stage_one = stage_one,
      fit = stage_two
    )
    class(out) <- "ivm"
  }
  return(out)
}
