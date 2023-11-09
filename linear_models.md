linear_models
================
Longyi Zhao
2023-11-09

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6, 
  out.width = "90%"
)

theme_set (theme_minimal() +theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis", 
  ggplots.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_disrete = scale_fill_viridis_d

set.seed(1)
```

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |>
  mutate(stars = review_scores_location/2) |>
  select(
    price, stars, borough = neighbourhood_group, 
    neighbourhood, room_type) |>
  filter(borough != "Staten Island")
```

Let’s fit a model!!

``` r
fit = lm(price ~ stars +borough, data = nyc_airbnb)
```

``` r
fit = nyc_airbnb |>
  lm(price~stars + borough, data = _)
```

take a look at the fit by using summary(fit) in the console Not very
useful methods

``` r
# summary(fit)
# summary(fit)$coef
# coef(fit)
# fitted.values(fit)
```

tidy up the output instead

``` r
fit |>
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

tidy up the coefficient, this is more useful

``` r
fit |>
  broom::tidy() |>
  mutate(term = str_replace(term, "^borough", "Borough: ")) |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

``` r
fit = nyc_airbnb |>
  mutate(borough = fct_infreq(borough), 
         room_type = fct_infreq(room_type))|>
  lm(price ~ stars +borough + room_type, data = _) 

fit |>
  broom :: tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.8       9.54 1.56e-21
    ## 2 stars                     21.9      2.43      9.01 2.09e-19
    ## 3 boroughBrooklyn          -40.3      2.15    -18.8  4.62e-78
    ## 4 boroughQueens            -55.5      3.59    -15.4  1.32e-53
    ## 5 boroughBronx             -63.0      8.22     -7.67 1.76e-14
    ## 6 room_typePrivate room   -105.       2.05    -51.2  0       
    ## 7 room_typeShared room    -129.       6.15    -21.0  2.24e-97

## Quick look at regression diagnostics

The modelr package can be used to add residuals and fitted values to a
dataframe can use modelr to manipulate for example plot the residual

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() 
```

    ## Warning: Removed 9962 rows containing non-finite values (`stat_ydensity()`).

<img src="linear_models_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  ggplot(aes(x = stars, y = resid)) +
  geom_point() 
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

<img src="linear_models_files/figure-gfm/unnamed-chunk-9-2.png" width="90%" />

## Hypothesis testing for caregorical predictor

fit a “null” and “alternative” model

``` r
fit_null = lm (price ~ stars + borough, data = nyc_airbnb)
fit_alternative = lm (price ~ stars + borough + room_type, data = nyc_airbnb)

anova(fit_null, fit_alternative) |>
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0

## Borough-level differences
