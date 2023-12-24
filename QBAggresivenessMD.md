QB Aggressiveness
================
Grant Hohol

## QB Agressiveness and Relative Success

Quantifying the aggresiveness of each Quarterback and their success in
terms of accuracy and completions relative to that aggresiveness.

Loading necessary packages

``` r
library(magrittr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::extract()   masks magrittr::extract()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ purrr::set_names() masks magrittr::set_names()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(nflfastR)
library(ranger) # modeling package
library(ggthemes)
library(ggimage)
library(gt)
library(vip)
```

    ## 
    ## Attaching package: 'vip'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

## Completion Percentage Over Expectation Model Creation

Loadig play by play data and create a clean data frame of pass plays
with the variables we care about relative to completion percentage.

``` r
pbp <- load_pbp(2020:2023)

# create new dataset of pass plays with no na values in the variables we care about
pass_plays <- pbp %>%
  filter(pass == 1) %>%
  filter(!is.na(air_yards), !is.na(down), !is.na(score_differential),
         !is.na(ydstogo), !is.na(half_seconds_remaining)) %>%
  # make the down variable a "factor" variable so that it is not just numeric
  mutate(down = as.factor(down))

# select the variables we want to weight against comeplete_pass
cpoe_model_data <- pass_plays %>%
  select(complete_pass, down, score_differential, ydstogo, 
         half_seconds_remaining, yardline_100, air_yards, qb_hit, goal_to_go)
```

Applying a linear regression model for the dependent variable of a
completed pass

``` r
# apply a linear regression model
cpoe_lm <- lm(complete_pass ~ down + score_differential + ydstogo +
              half_seconds_remaining + yardline_100 + air_yards + qb_hit +
               goal_to_go, data = cpoe_model_data)
```

What variables matter the most?

``` r
summary(cpoe_lm)
```

    ## 
    ## Call:
    ## lm(formula = complete_pass ~ down + score_differential + ydstogo + 
    ##     half_seconds_remaining + yardline_100 + air_yards + qb_hit + 
    ##     goal_to_go, data = cpoe_model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3629 -0.5091  0.2232  0.3223  1.1662 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             7.042e-01  7.025e-03 100.241  < 2e-16 ***
    ## down2                  -4.225e-03  4.017e-03  -1.052 0.292947    
    ## down3                  -3.707e-02  4.449e-03  -8.331  < 2e-16 ***
    ## down4                  -5.260e-02  1.165e-02  -4.516  6.3e-06 ***
    ## score_differential      4.850e-04  1.654e-04   2.933 0.003358 ** 
    ## ydstogo                 1.671e-03  4.515e-04   3.701 0.000215 ***
    ## half_seconds_remaining  9.547e-06  3.086e-06   3.093 0.001979 ** 
    ## yardline_100            1.141e-03  7.920e-05  14.402  < 2e-16 ***
    ## air_yards              -1.245e-02  1.669e-04 -74.582  < 2e-16 ***
    ## qb_hit                 -2.271e-01  5.790e-03 -39.219  < 2e-16 ***
    ## goal_to_go             -1.169e-01  9.150e-03 -12.779  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4525 on 73180 degrees of freedom
    ## Multiple R-squared:  0.1016, Adjusted R-squared:  0.1015 
    ## F-statistic: 827.6 on 10 and 73180 DF,  p-value: < 2.2e-16

``` r
vip(cpoe_lm)
```

![](QBAggresivenessMD_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Unsurprisingly, air yards, how far the ball travels in the air, matters
by far the most in terms of completion percentage.

Creating a data frame of expected completion percentages on each pass
play

``` r
# data frame of expected completion percentages for every play based on our
# previously made linear regression model
cp_preds <- data.frame(predict.lm(cpoe_lm, newdata=pass_plays)) %>%
  rename(exp_cp =   predict.lm.cpoe_lm..newdata...pass_plays.)
```

Creating an Air Yards Over Expected Regression Model

``` r
pass_plays_ayoe_model_data <- pass_plays %>%
  select(air_yards, down, score_differential, ydstogo, half_seconds_remaining, 
         qb_hit, yardline_100) 

air_yards_lm <- lm(air_yards ~ down + score_differential + ydstogo + 
                     half_seconds_remaining + qb_hit + yardline_100,
                   data = pass_plays_ayoe_model_data)

summary(air_yards_lm)
```

    ## 
    ## Call:
    ## lm(formula = air_yards ~ down + score_differential + ydstogo + 
    ##     half_seconds_remaining + qb_hit + yardline_100, data = pass_plays_ayoe_model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -64.187  -6.472  -2.367   4.226  56.404 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             5.604e+00  1.444e-01  38.800  < 2e-16 ***
    ## down2                  -4.463e-01  8.911e-02  -5.008 5.51e-07 ***
    ## down3                   1.391e+00  9.856e-02  14.114  < 2e-16 ***
    ## down4                   3.494e+00  2.581e-01  13.539  < 2e-16 ***
    ## score_differential      2.855e-03  3.670e-03   0.778    0.437    
    ## ydstogo                 8.676e-02  9.978e-03   8.695  < 2e-16 ***
    ## half_seconds_remaining -8.656e-04  6.843e-05 -12.650  < 2e-16 ***
    ## qb_hit                  1.910e+00  1.283e-01  14.888  < 2e-16 ***
    ## yardline_100            3.267e-02  1.617e-03  20.202  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.04 on 73182 degrees of freedom
    ## Multiple R-squared:  0.01856,    Adjusted R-squared:  0.01845 
    ## F-statistic:   173 on 8 and 73182 DF,  p-value: < 2.2e-16

``` r
# shows the importance of each variable
vip(air_yards_lm, num_features = 7)
```

![](QBAggresivenessMD_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Here, we see that whether the QB was hit matters again, but where the
offense is on the field is the most important factor. This makes sense
because the possible air yards gets smaller as an offense approaches the
endzone.

Creating a new data frame with Completion % Over Expected and Air Yards
Over Expected added

``` r
# takes in fitted linear regression model as main argument
# makes predictions based off of passed data
# gives us a vector so wrap it in a data frame
air_yard_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_plays)) %>%
  # change name of column in data frame
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_plays.)

new_pass_plays <- cbind(pass_plays, cp_preds, air_yard_preds)

# create cpoe column
new_pass_plays <- new_pass_plays %>%
  mutate(new_cpoe = complete_pass - exp_cp) %>%
  mutate(ayoe = air_yards - exp_air_yards) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
```

Creating another new data frame with just the QB name, their CPOE and
AYOE

``` r
# data frame of just QBs CPOE and AYOE
oe_by_passer <- new_pass_plays %>%
  group_by(passer) %>%
  summarize(passes = n(), avg_cpoe = mean(new_cpoe), avg_ayoe = mean(ayoe),
            team_logo = last(team_logo_espn)) %>%
    filter(passes >= 750) %>%
  filter(!(passer %in% c("Z.Wilson", "S.Darnold", "C.Wentz", "B.Mayfield", "J.Fields"))) %>%
  mutate(condition = ifelse(passer %in% c("T.Tagovailoa", "J.Herbert", "J.Burrow"), 1, 0.25)) %>%
  arrange(-avg_cpoe)
```

Make the graph

``` r
passers_combined <- oe_by_passer %>%
  filter(passer %in% c("T.Tagovailoa", "J.Herbert", "J.Burrow"))

oe_by_passer %>%
  ggplot(aes(x = avg_ayoe, y = avg_cpoe)) + 
  geom_hline(yintercept = mean(oe_by_passer$avg_cpoe), linetype = "dashed") + 
  geom_vline(xintercept = mean(oe_by_passer$avg_ayoe), linetype = "dashed") + 
  geom_image(data = passers_combined, aes(image = team_logo), size = 0.05, asp = 16/9) + # set image, size of the images, and aspect ratio of images SCATTERPLOT
  geom_point(shape = 21) +
  ggrepel::geom_text_repel(aes(label = passer), size = 4, alpha=oe_by_passer$condition) +
  labs(x = "Average Air Yards Over Expected",
       y = "Average Completion % Over Expected",
       title = "AYOE vs CPOE for NFL QBS",
       subtitle = "2020-2023") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(size = 14))
```

![](QBAggresivenessMD_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

From this graph we can see that Tua Tagovailoa has around the same
completion percentage over expected as Herbert, but he’s doing it on
throws further down the field, which is more impressive. Burrow has a
higher CPOE than Tua, but on throws less far down the field. The main
loser of this insight is Justin Herbert. He has the lowest CPOE while
also having the lowest AYOE.
