Effect of Population Density on Life Expectancy
================
Maria Benavides
2020-05-31

``` r
# Import data 
gapminder_df <- gapminder::gapminder
geonames_df <- GNcountryInfo() %>%
  rename(
    country = countryCode
  )

# Create key to join
gapminder_key <- gapminder_df %>%
  mutate(
    country = countrycode(
      country, 
      origin = "country.name", 
      destination = "iso2c"
    )
  )

# Merge dataframes based on the country variable
all_variables <- left_join(
  gapminder_key, geonames_df, by = "country"
  ) %>%
  select( # Select only variables of interest for this exercise
   country,
   lifeExp,
   pop,
   areaInSqKm, 
   year
  ) %>%
  mutate(
    areaInSqKm = as.numeric(areaInSqKm))

# Add new variable for pop density 
all_variables <- all_variables %>%
  mutate(
    pop_density = pop / areaInSqKm
  )
```

``` r
# Run a regression model 
regression_popden_lifeexp <- lm(lifeExp ~ pop_density, data = all_variables)
regr_table <- tidy(regression_popden_lifeexp) %>%
  rename(
    "Predictor" = term,
    "B" = estimate, 
    "SE" = std.error, 
    "t" = statistic, 
    "p" = p.value
    ) 
kable(regr_table, 
      digits = 3, 
      caption = "Table correlation between life expectancy and population density"
      ) 
```

<table>

<caption>

Table correlation between life expectancy and population density

</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor

</th>

<th style="text-align:right;">

B

</th>

<th style="text-align:right;">

SE

</th>

<th style="text-align:right;">

t

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:right;">

58.748

</td>

<td style="text-align:right;">

0.318

</td>

<td style="text-align:right;">

184.616

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

pop\_density

</td>

<td style="text-align:right;">

0.005

</td>

<td style="text-align:right;">

0.001

</td>

<td style="text-align:right;">

8.508

</td>

<td style="text-align:right;">

0

</td>

</tr>

</tbody>

</table>

Using a simple OLS model that considers life expectancy as the dependent
variable and population density as the independent, we see that there is
a positive correlation between those, which is statistically
significant; this suggests that places with higher population density
have a higher life expectancy.

``` r
#Plot the correlation
ggplot(all_variables, aes(x=pop_density, y=lifeExp)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method=lm)+
  labs(
    title = "Correlation between life expectancy and population density", 
    x = "Population density", 
    y = "Life expectancy"
  )
```

    ## `geom_smooth()` using formula 'y ~ x'

![](gapminder_files/figure-gfm/correlation%20graph-1.png)<!-- -->

The graph is shows visually what the model predicted, that is to say,
there is a positive significant correlation between life expectancy and
population density. Nonetheless, it is worth mentioning that this graph
is showing us that there are outliers in our data frame. Singapore and
Hong Kong are extremely dense, and might biases these
    results.

## Session info

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                               
    ##  version  R version 3.6.3 (2020-02-29)        
    ##  os       Red Hat Enterprise Linux 8.1 (Ootpa)
    ##  system   x86_64, linux-gnu                   
    ##  ui       X11                                 
    ##  language (EN)                                
    ##  collate  en_US.UTF-8                         
    ##  ctype    en_US.UTF-8                         
    ##  tz       America/Chicago                     
    ##  date     2020-05-31                          
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [2] CRAN (R 3.6.3)
    ##  backports     1.1.5   2019-10-02 [2] CRAN (R 3.6.3)
    ##  broom       * 0.5.6   2020-04-20 [1] CRAN (R 3.6.3)
    ##  callr         3.4.3   2020-03-28 [2] CRAN (R 3.6.3)
    ##  cellranger    1.1.0   2016-07-27 [2] CRAN (R 3.6.3)
    ##  cli           2.0.2   2020-02-28 [2] CRAN (R 3.6.3)
    ##  colorspace    1.4-1   2019-03-18 [2] CRAN (R 3.6.3)
    ##  countrycode * 1.2.0   2020-05-22 [1] CRAN (R 3.6.3)
    ##  crayon        1.3.4   2017-09-16 [2] CRAN (R 3.6.3)
    ##  DBI           1.1.0   2019-12-15 [2] CRAN (R 3.6.3)
    ##  dbplyr        1.4.2   2019-06-17 [2] CRAN (R 3.6.3)
    ##  desc          1.2.0   2018-05-01 [2] CRAN (R 3.6.3)
    ##  devtools      2.3.0   2020-04-10 [1] CRAN (R 3.6.3)
    ##  digest        0.6.25  2020-02-23 [2] CRAN (R 3.6.3)
    ##  dplyr       * 0.8.5   2020-03-07 [2] CRAN (R 3.6.3)
    ##  ellipsis      0.3.0   2019-09-20 [2] CRAN (R 3.6.3)
    ##  evaluate      0.14    2019-05-28 [2] CRAN (R 3.6.3)
    ##  fansi         0.4.1   2020-01-08 [2] CRAN (R 3.6.3)
    ##  farver        2.0.3   2020-01-16 [2] CRAN (R 3.6.3)
    ##  forcats     * 0.5.0   2020-03-01 [2] CRAN (R 3.6.3)
    ##  fs            1.4.0   2020-03-31 [2] CRAN (R 3.6.3)
    ##  gapminder     0.3.0   2017-10-31 [2] CRAN (R 3.6.3)
    ##  generics      0.0.2   2018-11-29 [2] CRAN (R 3.6.3)
    ##  geonames    * 0.999   2019-02-19 [1] CRAN (R 3.6.3)
    ##  ggplot2     * 3.3.0   2020-03-05 [2] CRAN (R 3.6.3)
    ##  glue          1.4.0   2020-04-03 [1] CRAN (R 3.6.3)
    ##  gtable        0.3.0   2019-03-25 [2] CRAN (R 3.6.3)
    ##  haven         2.2.0   2019-11-08 [2] CRAN (R 3.6.3)
    ##  here        * 0.1     2017-05-28 [1] CRAN (R 3.6.3)
    ##  highr         0.8     2019-03-20 [2] CRAN (R 3.6.3)
    ##  hms           0.5.3   2020-01-08 [2] CRAN (R 3.6.3)
    ##  htmltools     0.4.0   2019-10-04 [2] CRAN (R 3.6.3)
    ##  httr          1.4.1   2019-08-05 [2] CRAN (R 3.6.3)
    ##  jsonlite      1.6.1   2020-02-02 [2] CRAN (R 3.6.3)
    ##  kableExtra  * 1.1.0   2019-03-16 [1] CRAN (R 3.6.3)
    ##  knitr         1.28    2020-02-06 [2] CRAN (R 3.6.3)
    ##  labeling      0.3     2014-08-23 [2] CRAN (R 3.6.3)
    ##  lattice       0.20-38 2018-11-04 [2] CRAN (R 3.6.3)
    ##  lifecycle     0.2.0   2020-03-06 [2] CRAN (R 3.6.3)
    ##  lubridate     1.7.8   2020-04-06 [1] CRAN (R 3.6.3)
    ##  magrittr      1.5     2014-11-22 [2] CRAN (R 3.6.3)
    ##  Matrix        1.2-18  2019-11-27 [2] CRAN (R 3.6.3)
    ##  memoise       1.1.0   2017-04-21 [2] CRAN (R 3.6.3)
    ##  mgcv          1.8-31  2019-11-09 [2] CRAN (R 3.6.3)
    ##  modelr        0.1.6   2020-02-22 [2] CRAN (R 3.6.3)
    ##  munsell       0.5.0   2018-06-12 [2] CRAN (R 3.6.3)
    ##  nlme          3.1-144 2020-02-06 [2] CRAN (R 3.6.3)
    ##  pillar        1.4.3   2019-12-20 [2] CRAN (R 3.6.3)
    ##  pkgbuild      1.0.6   2019-10-09 [2] CRAN (R 3.6.3)
    ##  pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 3.6.3)
    ##  pkgload       1.0.2   2018-10-29 [2] CRAN (R 3.6.3)
    ##  prettyunits   1.1.1   2020-01-24 [2] CRAN (R 3.6.3)
    ##  processx      3.4.2   2020-02-09 [2] CRAN (R 3.6.3)
    ##  ps            1.3.2   2020-02-13 [2] CRAN (R 3.6.3)
    ##  purrr       * 0.3.3   2019-10-18 [2] CRAN (R 3.6.3)
    ##  R6            2.4.1   2019-11-12 [2] CRAN (R 3.6.3)
    ##  Rcpp          1.0.4   2020-03-17 [2] CRAN (R 3.6.3)
    ##  readr       * 1.3.1   2018-12-21 [2] CRAN (R 3.6.3)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.3)
    ##  remotes       2.1.1   2020-02-15 [2] CRAN (R 3.6.3)
    ##  reprex        0.3.0   2019-05-16 [2] CRAN (R 3.6.3)
    ##  rjson         0.2.20  2018-06-08 [2] CRAN (R 3.6.3)
    ##  rlang         0.4.5   2020-03-01 [2] CRAN (R 3.6.3)
    ##  rmarkdown     2.1     2020-01-20 [2] CRAN (R 3.6.3)
    ##  rprojroot     1.3-2   2018-01-03 [2] CRAN (R 3.6.3)
    ##  rstudioapi    0.11    2020-02-07 [2] CRAN (R 3.6.3)
    ##  rvest         0.3.5   2019-11-08 [2] CRAN (R 3.6.3)
    ##  scales        1.1.0   2019-11-18 [2] CRAN (R 3.6.3)
    ##  sessioninfo   1.1.1   2018-11-05 [2] CRAN (R 3.6.3)
    ##  stringi       1.4.6   2020-02-17 [2] CRAN (R 3.6.3)
    ##  stringr     * 1.4.0   2019-02-10 [2] CRAN (R 3.6.3)
    ##  testthat      2.3.2   2020-03-02 [2] CRAN (R 3.6.3)
    ##  tibble      * 3.0.0   2020-03-30 [2] CRAN (R 3.6.3)
    ##  tidyr       * 1.0.2   2020-01-24 [2] CRAN (R 3.6.3)
    ##  tidyselect    1.0.0   2020-01-27 [2] CRAN (R 3.6.3)
    ##  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 3.6.3)
    ##  usethis       1.6.0   2020-04-09 [1] CRAN (R 3.6.3)
    ##  vctrs         0.2.4   2020-03-10 [2] CRAN (R 3.6.3)
    ##  viridisLite   0.3.0   2018-02-01 [2] CRAN (R 3.6.3)
    ##  webshot       0.5.2   2019-11-22 [1] CRAN (R 3.6.3)
    ##  withr         2.1.2   2018-03-15 [2] CRAN (R 3.6.3)
    ##  xfun          0.12    2020-01-13 [2] CRAN (R 3.6.3)
    ##  xml2          1.3.0   2020-04-01 [2] CRAN (R 3.6.3)
    ##  yaml          2.2.1   2020-02-01 [2] CRAN (R 3.6.3)
    ## 
    ## [1] /home/mabenavides/R/x86_64-redhat-linux-gnu-library/3.6
    ## [2] /usr/lib64/R/library
    ## [3] /usr/share/R/library
