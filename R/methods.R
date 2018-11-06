#' Model matrix for ivm
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
model.matrix.ivm <- function(object, ...) model.matrix(object$fit)


#' Model frame for ivm
#'
#' @param formula ivm model object.
#' @param ... other arguments
#' @export
model.frame.ivm <- function(formula, ...) model.frame(formula$fit)


#' Model family for ivm
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
family.ivm <- function(object, ...) family(object$fit)


#' Model summary for ivm
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
summary.ivm <- function(object, ...) summary(object$fit)


#' Model print for ivm
#'
#' @param x ivm model object.
#' @param ... other arguments
#' @export
print.ivm <- function(x, ...) print(x$fit)


#' Number of observations in ivm model
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
nobs.ivm <- function(object, ...) nobs(object$fit)


#' Plot ivm model
#'
#' @param x ivm model object.
#' @param ... other arguments
#' @export
plot.ivm <- function(x, ...) {
  cat("stage 1 \n")
  plot(x$stage_one)
  cat("stage 2 \n")
  plot(x$fit)
}


#' Make predictions from ivm model
#'
#' @param object ivm model object.
#' @param newdata (optional) new data for predictions.
#' @param ... other arguments.
#' @export
predict.ivm <- function(object, newdata = NULL, ...) {
  stage_one_pred <- predict(object$stage_one, newdata = newdata)
  model_data <- model.frame(formula(object$fit), data = newdata)
  model_data[, which(names(model_data) == object$instrumented)] <- stage_one_pred$fitted.values
  stage_two_pred <- predict(object$fit, data = model_data)
  return(stage_two_pred)
}


#' Get residuals from ivm model
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
residuals.ivm <- function(object, ...) {
  out <- list(
    stage_one_residuals = object$stage_one$residuals,
    stage_two_residuals = object$fit$residuals
  )
  return(out)
}


#' Get variables in  ivm model
#'
#' @param object ivm model object.
#' @param ... other arguments
#' @export
variable.names.ivm <- function(object, ...) {
  out <- list(
    stage_one_vars = variable.names(object$stage_one),
    stage_two_vars = variable.names(object$fit)
  )
  return(out)
}

