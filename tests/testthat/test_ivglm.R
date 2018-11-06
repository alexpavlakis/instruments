context("IV GLM")
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
bin <- sample(0:1, size = N, replace = TRUE)

test_that("iv.glm is glm when instrument formula is null", {
  ivglm1 <- iv.glm(model_formula = bin ~ x + z, family = binomial, link = 'probit')
  glm1 <- glm(formula = bin ~ x + z, family = binomial(link = 'probit'))
  expect_identical(round(ivglm1$fitted.values, 2), round(glm1$fitted.values, 2))
})

test_that("iv.glm identifies instruments and instrumented", {
  ivglm1 <- iv.glm(model_formula = bin ~ x + z, instrument_formula = z ~ p + d + q,
                   family = binomial, link = 'logit')
  expect_equal(ivglm1$instruments, c("p", "d", "q"))
  expect_equal(ivglm1$instrumented, c("z"))
})

test_that("iv.lm throws an error for too many istruments", {
  expect_error(iv.glm(model_formula = bin ~ x + z, z ~ p + d + q, x ~ d + q,
                      family = binomial, link = "probit"))
})