<!-- README.md is generated from README.Rmd. Please edit that file -->
instruments
===========

Instruments makes it easy to fit instrumental variable regression models. It has two core functions:

1.  `iv.lm` for estimating linear regression models with instrumental variables;
2.  `iv.glm` for estimating generalized linear models with instrumental variables.

Linear models
-------------

First we'll create some fake data to demonstrate the linear model. Suppose you want to estimate the impact of a subscription program on your customers' spending, but you know that customers who tend to spend more also tend to sign up for subscriptions. How can you disentangle those effects?

Suppose that you also offer incentives (free samples, buy-one-get-one-free, etc) to encourage customers to enroll in subscriptions, and that these incentives are offered at random. You may be able to use incentives as an instrumental variable to isolate the impact of subscriptions on spending.

``` r
# Fake data
set.seed(56)
N <- 10e4
baseline_spending <- rpois(N, 10)
incentive <- sample(c(0, 1), size = N, replace = TRUE)
subscription <- rbinom(N, 1, p = 1/(1 + exp(-(baseline_spending + incentive - 10))))
total_spending <- baseline_spending + 2*subscription + rnorm(N, 0, 1)
```

The true impact of a subscription on total spending is 2. Since the unobserved variable baseline speding propensity also impacts whether someone gets a subscription, we cannot identify the impact of a subscription on spending with an ordinary linear regression. The ordinary linear regression will tend to overestimate the impact of a subscription on spending.

``` r
lm_ols <- lm(formula = total_spending ~ subscription)
summary(lm_ols)
#> 
#> Call:
#> lm(formula = total_spending ~ subscription)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -9.2597 -1.7086 -0.0683  1.5953 15.7069 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   7.67147    0.01173   654.2   <2e-16 ***
#> subscription  6.32733    0.01597   396.2   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.517 on 99998 degrees of freedom
#> Multiple R-squared:  0.6109, Adjusted R-squared:  0.6109 
#> F-statistic: 1.57e+05 on 1 and 99998 DF,  p-value: < 2.2e-16
```

Instead we can use an instrumental variable regression, with `incentive` as the instrument.

``` r
library(instruments)

lm_2sls <- iv.lm(model_formula = total_spending ~ subscription,
                 instrument_formula = subscription ~ incentive)
#> correlation between subscription and incentive: 0.106
#> correlation between incentive and residuals: 0

summary(lm_2sls)
#> 
#> Call:
#> lm(formula = as.formula(deparse(model_formula)), data = model_data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -12.5684  -3.1502  -0.0308   2.9728  18.5190 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   10.0276     0.1311  76.508  < 2e-16 ***
#> subscription   1.9582     0.2419   8.095 5.77e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 4.034 on 99998 degrees of freedom
#> Multiple R-squared:  0.0006549,  Adjusted R-squared:  0.0006449 
#> F-statistic: 65.53 on 1 and 99998 DF,  p-value: 5.773e-16
```

Generalized Linear Models
-------------------------

Suppose instead that we want to estimate the impact of subscriptions on customer churn. Continuing our fake data example from above, we define churn as a function of both subscription and baseline spending.

``` r
churn <- rbinom(N, 1, 1/(1 + exp(-(-10 + baseline_spending + 2*subscription + rnorm(N, 0, 1)))))
```

Since churn is a binary variable (0, 1), a linear regression is not appropriate. Instead we will estimate this model with a logistic regression using the `glm` function in base R and `iv.glm` function in instruments. As before, the simple `glm` function will overestimate the impact of subscription on churn due to the correlation between baseline spending rates and having a subscription.

``` r
glm_logit <- glm(formula = churn ~ subscription, family = binomial(link = 'logit'))
glm_iv <- iv.glm(model_formula = churn ~ subscription, 
                 instrument_formula = subscription ~ incentive, 
                 family = binomial, link = 'logit')
#> correlation between subscription and incentive: 0.106
#> correlation between incentive and residuals: 0

summary(glm_logit)
#> 
#> Call:
#> glm(formula = churn ~ subscription, family = binomial(link = "logit"))
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -2.1384  -0.6808   0.4630   0.4630   1.7752  
#> 
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -1.34394    0.01150  -116.8   <2e-16 ***
#> subscription  3.52308    0.01831   192.4   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 136074  on 99999  degrees of freedom
#> Residual deviance:  82422  on 99998  degrees of freedom
#> AIC: 82426
#> 
#> Number of Fisher Scoring iterations: 4
summary(glm_iv)
#> 
#> Call:
#> glm(formula = model_formula, family = family(link = link), data = model_data)
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#> -1.342  -1.291   1.021   1.067   1.067  
#> 
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.26951    0.06579  -4.096  4.2e-05 ***
#> subscription  1.09699    0.12154   9.026  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 136074  on 99999  degrees of freedom
#> Residual deviance: 135992  on 99998  degrees of freedom
#> AIC: 135996
#> 
#> Number of Fisher Scoring iterations: 4
```

Diagnostics
-----------

You've probably noticed that `iv.lm` and `iv.glm` print the correlation between the independent variable and the instrument and the correlation betweent the instrument and the regression error term. These two metrics help understand whether the model is valid.

Instrumental variable regression models rely on two assumptions:

1.  Exclusion restriction: the instrument is not correlated with the error term;
2.  Validity: the instrument is correlated with the endogenous explanatory variable (subscription, in our example).

We can also use the `diagnose` funcion to automatically evaluate a fit `iv` model. If it returns nothing, that means that your instrumental variable model seems valid.

``` r
diagnose(glm_iv)
```
