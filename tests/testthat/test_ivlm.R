context("IV LM")
library(instruments)

# Fake data
set.seed(56)
N <- 10e3
x <- rnorm(N, 0, 1)
y <- rnorm(N, 0, 1)
z <- rnorm(N, 0, 1)
p <- rnorm(N, 0, 1)
d <- rnorm(N, 0, 1)
q <- rnorm(N, 0, 1)

test_that("iv.lm is lm when instrument formula is null", {
  ivlm1 <- iv.lm(model_formula = y ~ x + z)
  lm1 <- lm(formula = y ~ x + z)
  expect_identical(ivlm1$fitted.values, lm1$fitted.values)
})

test_that("iv.lm identifies instruments and instrumented", {
  ivlm1 <- iv.lm(model_formula = y ~ x + z, instrument_formula = z ~ p + d + q)
  expect_equal(ivlm1$instruments, c("p", "d", "q"))
  expect_equal(ivlm1$instrumented, c("z"))
})

test_that("iv.lm throws an error for too many istruments", {
  expect_error(iv.lm(model_formula = y ~ x + z, z ~ p + d + q, x ~ d + q))
})
