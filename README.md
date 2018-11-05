<!-- README.md is generated from README.Rmd. Please edit that file -->
instruments
===========

Instruments makes it easy to fit instrumental variable regression models. It has two core functions:

1.  `iv.lm` for estimating linear regression models with instrumental variables;
2.  `iv.glm` for estimating generalized linear models with instrumental variables.

Linear models
-------------

First we'll create some fake data to demonstrate the linear model. Suppose you want to estimate the impact of a subscription program on your customers' spending, but you know that customers who tend to spend more also tend to sign up for subscriptions. How can you disentangle those effects?

Suppose that you also offer discounts to incentivize customers to enroll in subscriptions, and that these discounts are offered at random. You may be able to use discounts as an instrumental variable to isolate the impact of subscriptions on spending.

``` r
# Fake data
set.seed(56)
N <- 10e4
baseline_spending <- rpois(N, 10)
discount <- rbeta(N, 1, 1)
subscription <- rbinom(N, 1, p = 1/(1 + exp(-(baseline_spending + discount - 10))))
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
#> -9.4506 -1.7008 -0.0772  1.5861 16.6921 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   7.63825    0.01167   654.5   <2e-16 ***
#> subscription  6.37799    0.01586   402.2   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.499 on 99998 degrees of freedom
#> Multiple R-squared:  0.6179, Adjusted R-squared:  0.6179 
#> F-statistic: 1.617e+05 on 1 and 99998 DF,  p-value: < 2.2e-16
```

Instead we can use an instrumental variable regression, with `discount` as the instrument.

``` r
library(instruments)

lm_2sls <- iv.lm(formula = total_spending ~ instrument,
                 instrument_formula = subscription ~ discount)
#> correlation between subscription and discount: 0.066
#> correlation between discount and residuals: 0

summary(lm_2sls)
#> 
#> Call:
#> lm(formula = as.formula(deparse(formula)), data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -12.466  -3.181   0.000   2.974  19.504 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   9.8754     0.2095  47.140  < 2e-16 ***
#> instrument    2.2469     0.3861   5.819 5.94e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 4.042 on 99998 degrees of freedom
#> Multiple R-squared:  0.0003385,  Adjusted R-squared:  0.0003285 
#> F-statistic: 33.86 on 1 and 99998 DF,  p-value: 5.939e-09
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
glm_iv <- iv.glm(formula = churn ~ instrument, 
                 instrument_formula = subscription ~ discount, 
                 family = binomial, link = 'logit')
#> correlation between subscription and discount: 0.066
#> correlation between discount and residuals: 0

summary(glm_logit)
#> 
#> Call:
#> glm(formula = churn ~ subscription, family = binomial(link = "logit"))
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -2.1492  -0.6738   0.4574   0.4574   1.7856  
#> 
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -1.36721    0.01161  -117.8   <2e-16 ***
#> subscription  3.57213    0.01847   193.4   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 136003  on 99999  degrees of freedom
#> Residual deviance:  81315  on 99998  degrees of freedom
#> AIC: 81319
#> 
#> Number of Fisher Scoring iterations: 4
summary(glm_iv)
#> 
#> Call:
#> glm(formula = as.formula(deparse(formula)), family = family(link = link), 
#>     data = data)
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#> -1.351  -1.312   1.021   1.046   1.072  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.3608     0.1050  -3.435 0.000593 ***
#> instrument    1.2690     0.1937   6.552 5.68e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 136003  on 99999  degrees of freedom
#> Residual deviance: 135960  on 99998  degrees of freedom
#> AIC: 135964
#> 
#> Number of Fisher Scoring iterations: 4
```

Diagnostics
-----------

You've probably noticed that `iv.lm` and `iv.glm` print the correlation between the independent variable and the instrument and the correlation betweent the instrument and the regression error term. These two metrics help understand whether the model is valid.

Instrumental variable regression models rely on two assumptions:

1.  Exclusion restriction: the instrument is not correlated with the error term;
2.  Validity: the instrument is correlated with the endogenous explanatory variable (subscription, in our example).

We can also use the `diagnose` funcion to automatically evaluate a fit `iv` model.

``` r
diagnose(glm_iv)
#> 0.066 correlation between discount and subscription. discount may not be a valid instrument 
#> 
```
