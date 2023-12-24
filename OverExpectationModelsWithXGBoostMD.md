Data Analysis of 2020 NFL QB Draft Class
================
Grant Hohol

## Over Expectation Models with XGBoost

We are going to attempt to quantify the amount of help that each
quarterback is getting from their surrounding cast by creating an
expected rushing yards metric and an expected yards after the catch
metric. By comparing each quarterbacks surrounding cast against these
expectation metrics, we can see whose supporting cast has played above
expectation the most/least.

First we need to load the needed packages.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(nflfastR)
library(ggthemes)
library(ranger)
library(ggimage)
library(vip)
```

    ## 
    ## Attaching package: 'vip'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(xgboost)
```

    ## 
    ## Attaching package: 'xgboost'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1
    ## Type rfNews() to see new features/changes/bug fixes.
    ## 
    ## Attaching package: 'randomForest'
    ## 
    ## The following object is masked from 'package:ranger':
    ## 
    ##     importance
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

Avoid scientific notation

``` r
options(scipen = 9999) # avoids scientific notation
```

## Cleaning and mutating data

Load play by play data for 2020-2023 seasons from nflFastR package

``` r
pbp <- load_pbp(2020:2023)
```

First we will be making the rushing yards over expectation metric, so we
need to filter and create a data frame of only rushing attempts. We will
also create a data frame to calculate each teams defensive yards per
carry allowed for the relative season so that defensive strength is
taken into account for our model.

``` r
# create df with only rush attempts from a non QB
rush_attempts <- pbp %>%
  filter(rush_attempt == 1, qb_scramble == 0, qb_dropback == 0, 
         !is.na(yards_gained))

# create df of defenses per season ypc
def_ypc <- rush_attempts %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam, season) %>%
  summarize(def_ypc = mean(yards_gained))
```

    ## `summarise()` has grouped output by 'defteam'. You can override using the
    ## `.groups` argument.

``` r
rush_attempts <- rush_attempts %>%
  left_join(def_ypc, by = c("defteam", "season"))
```

Now we will filter for the variables we want to include in our model.

``` r
rushing_data_join <- rush_attempts %>%
  select(label = yards_gained, yardline_100, half_seconds_remaining, 
         qtr, down, ydstogo, shotgun, no_huddle, ep, wp, def_ypc,
         rusher_player_name, posteam, defteam) %>%
  filter(!is.na(label), !is.na(down))

rushes <- rushing_data_join %>%
  select(-rusher_player_name, -posteam, -defteam)
```

``` r
str(rushes)
```

    ## nflvrs_d [53,393 × 11] (S3: nflverse_data/tbl_df/tbl/data.table/data.frame)
    ##  $ label                 : num [1:53393] 14 2 -6 0 2 9 7 3 9 3 ...
    ##  $ yardline_100          : num [1:53393] 55 41 39 64 78 62 53 22 19 10 ...
    ##  $ half_seconds_remaining: num [1:53393] 1782 1739 1701 1376 1217 ...
    ##  $ qtr                   : num [1:53393] 1 1 1 1 1 1 1 1 1 2 ...
    ##  $ down                  : num [1:53393] 1 1 2 2 1 1 2 1 2 1 ...
    ##  $ ydstogo               : num [1:53393] 10 10 8 5 10 10 1 10 7 10 ...
    ##  $ shotgun               : num [1:53393] 1 0 1 0 1 1 0 0 0 0 ...
    ##  $ no_huddle             : num [1:53393] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ep                    : num [1:53393] 2.769 3.626 3.171 1.876 0.477 ...
    ##  $ wp                    : num [1:53393] 0.58 0.604 0.6 0.805 0.408 ...
    ##  $ def_ypc               : num [1:53393] 4.3 4.3 4.3 4.3 3.59 ...
    ##  - attr(*, "nflverse_timestamp")= POSIXct[1:1], format: "2023-12-22 03:05:51"
    ##  - attr(*, "nflverse_type")= chr "play by play data"

From the output above, we see that the columns down, shotgun, and no
huddle are treated as numerics, but for the purpose of our models they
should be treated as factors, so we must convert them.

``` r
#converting numeric columns to factors
# need to convert numeric values to factors before training
rushes$down <- as.factor(rushes$down)
rushes$shotgun <- as.factor(rushes$shotgun)
rushes$no_huddle <- as.factor(rushes$no_huddle)

# one hot encoding 
# creates individual column for down 1, down 2, down 3 etc with 0 and 1 binary indicators 
dummy <- dummyVars(" ~ .", data = rushes)
rushing_model_data <- data.frame(predict(dummy, newdata = rushes))
```

## Creating Model

Now that the data set is cleaned and formatted as wanted, we can take
our training data and testing data.

``` r
sample_size <- floor(0.75 * nrow(rushing_model_data)) 
set.seed(1234)
# takes random 75% of rows from df
idx <- sample(seq_len(nrow(rushing_model_data)), size = sample_size) 
# create matrix with the selected 75% 
train_data <- as.matrix(rushing_model_data[idx, ]) 
# create matrix with other 25%
test_data <- as.matrix(rushing_model_data[-idx, ]) 
```

View dimensions of training data

``` r
dim(train_data)
```

    ## [1] 40044    16

Now we can create our model using XGBoost. These parameters have been
tuned to minimize the squared error of the model and avoid overfitting.

``` r
# Sample Parameter Tuning
ryoe_model <- xgboost(
  data = train_data[, 2:16],
  label = train_data[, 1],
  objective = "reg:squarederror",
  nrounds = 60,
  max_depth = 5,                # Increase the maximum depth of each tree
  eta = 0.1,                    # The learning rate
  subsample = 0.8,              # Use a fraction of the training data for each boosting round
  colsample_bytree = 0.8,       # Use a fraction of features for each tree
  min_child_weight = 1,         # Minimum sum of instance weight (Hessian) needed in a child
  gamma = 0.2,                  # Minimum loss reduction required to make a further partition on a leaf node
  lambda = 1,                   # L2 regularization term on weights
  alpha = 0.5                   # L1 regularization term on weights
)
```

    ## [1]  train-rmse:6.856127 
    ## [2]  train-rmse:6.687871 
    ## [3]  train-rmse:6.545888 
    ## [4]  train-rmse:6.429227 
    ## [5]  train-rmse:6.332745 
    ## [6]  train-rmse:6.252951 
    ## [7]  train-rmse:6.185596 
    ## [8]  train-rmse:6.129935 
    ## [9]  train-rmse:6.084446 
    ## [10] train-rmse:6.044379 
    ## [11] train-rmse:6.013434 
    ## [12] train-rmse:5.988570 
    ## [13] train-rmse:5.966648 
    ## [14] train-rmse:5.948785 
    ## [15] train-rmse:5.933603 
    ## [16] train-rmse:5.920374 
    ## [17] train-rmse:5.909526 
    ## [18] train-rmse:5.901484 
    ## [19] train-rmse:5.893583 
    ## [20] train-rmse:5.886791 
    ## [21] train-rmse:5.878313 
    ## [22] train-rmse:5.873332 
    ## [23] train-rmse:5.867434 
    ## [24] train-rmse:5.863191 
    ## [25] train-rmse:5.857181 
    ## [26] train-rmse:5.853609 
    ## [27] train-rmse:5.850750 
    ## [28] train-rmse:5.844663 
    ## [29] train-rmse:5.842541 
    ## [30] train-rmse:5.837285 
    ## [31] train-rmse:5.831337 
    ## [32] train-rmse:5.828536 
    ## [33] train-rmse:5.824484 
    ## [34] train-rmse:5.819106 
    ## [35] train-rmse:5.817005 
    ## [36] train-rmse:5.811767 
    ## [37] train-rmse:5.808286 
    ## [38] train-rmse:5.806798 
    ## [39] train-rmse:5.801588 
    ## [40] train-rmse:5.797856 
    ## [41] train-rmse:5.794229 
    ## [42] train-rmse:5.793255 
    ## [43] train-rmse:5.788201 
    ## [44] train-rmse:5.784738 
    ## [45] train-rmse:5.779441 
    ## [46] train-rmse:5.776676 
    ## [47] train-rmse:5.774420 
    ## [48] train-rmse:5.770800 
    ## [49] train-rmse:5.767031 
    ## [50] train-rmse:5.766047 
    ## [51] train-rmse:5.761316 
    ## [52] train-rmse:5.756532 
    ## [53] train-rmse:5.752822 
    ## [54] train-rmse:5.747542 
    ## [55] train-rmse:5.744060 
    ## [56] train-rmse:5.737434 
    ## [57] train-rmse:5.732439 
    ## [58] train-rmse:5.731836 
    ## [59] train-rmse:5.729208 
    ## [60] train-rmse:5.726665

Using vip, we can view the importance or weight of each independent
variable

``` r
vip(ryoe_model)
```

![](OverExpectationModelsWithXGBoostMD_files/figure-gfm/ryoe_model-1.png)<!-- -->

Interestingly, the yardline the offense is at has the highest affect on
expected rushing yards. This is likely because it becomes harder to run
as teams get closer to the endzone due to a tightened defense.

Model Testing

``` r
pred_xgb <- predict(ryoe_model, test_data[, 2:16])
yhat <- pred_xgb
y <- test_data[, 1] 
postResample(yhat, y)
```

    ##      RMSE  Rsquared       MAE 
    ## 6.1015483 0.0473694 3.5521864

## Mutate More Data for Display and Graph Purposes

Add expected rushing yards as a column to original data frame

``` r
rushing_preds <- as.data.frame(
  matrix(predict(ryoe_model, as.matrix(rushing_model_data %>% select(-label)))) 
) %>%
  dplyr::rename(exp_yards = V1)

ryoe_projs <- cbind(rushing_data_join, rushing_preds)
```

Edit data frame to include only players from the teams we care about and
those players respective mean rushing yards over expected.

``` r
ryoe_projs <- ryoe_projs %>%
  mutate(ryoe = label - exp_yards) %>%
  group_by(rusher_player_name) %>%
  summarize(rushes = n(), 
            mean_ryoe = mean(ryoe), team = last(posteam)) %>%
  filter(team %in% c("MIA", "CIN", "LAC")) %>%
  filter(!(rusher_player_name %in% c("T.Tagovailoa", "J.Burrow"))) %>%
  group_by(team) %>%
  arrange(-mean_ryoe)

head(ryoe_projs)
```

    ## # A tibble: 6 × 4
    ## # Groups:   team [3]
    ##   rusher_player_name rushes mean_ryoe team 
    ##   <chr>               <int>     <dbl> <chr>
    ## 1 S.Williams              2     13.2  CIN  
    ## 2 T.Irwin                 1      5.80 CIN  
    ## 3 T.Boyd                 10      4.20 CIN  
    ## 4 B.Berrios              20      4.13 MIA  
    ## 5 D.Achane               72      3.69 MIA  
    ## 6 D.Davis                 9      3.36 LAC

Separate that data frame into team specific data frames

``` r
mia <- ryoe_projs %>%
  filter(team == "MIA")
cin <- ryoe_projs %>%
  filter(team == "CIN")
lac <- ryoe_projs %>%
  filter(team == "LAC")

mia <- mia %>%
  mutate(mean = sum(rushes * mean_ryoe)/sum(rushes))
cin <- cin %>%
  mutate(mean = sum(rushes * mean_ryoe )/ sum(rushes))
lac <- lac %>%
  mutate(mean = sum(rushes * mean_ryoe) / sum(rushes))
```

Group and average by team so that the mean_ryoe is an average for the
team since we don’t care about individual statistics, just the
supporting cast as a whole. Adding team color and logo columns for
graphing purposes.

``` r
team_ryoe <- data.frame(
 team = c("MIA", "CIN", "LAC"), 
 mean_ryoe = c(0.264, -0.176, -0.271),
 team_color = c("#008E97", "#FB4F14", "#007BC7"), 
 logo = c("https://a.espncdn.com/i/teamlogos/nfl/500/mia.png", 
          "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png", 
          "https://a.espncdn.com/i/teamlogos/nfl/500/lac.png"))
head(team_ryoe)
```

    ##   team mean_ryoe team_color                                              logo
    ## 1  MIA     0.264    #008E97 https://a.espncdn.com/i/teamlogos/nfl/500/mia.png
    ## 2  CIN    -0.176    #FB4F14 https://a.espncdn.com/i/teamlogos/nfl/500/cin.png
    ## 3  LAC    -0.271    #007BC7 https://a.espncdn.com/i/teamlogos/nfl/500/lac.png

## Graph

Time to plot

``` r
ggplot(team_ryoe, aes(x = team, y = mean_ryoe, fill = team_color)) + 
  scale_color_identity(aesthetics = "fill") + 
  geom_bar(stat = "identity") + 
  geom_image(aes(image = logo), asp = 16/9, size = 0.1) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + 
  labs(title = "Mean Rushing Yards Over Expected Per Rush", 
       subtitle = "2020-2023", 
       x = "Team", 
       y = "Rushing Yards Over Expected") + 
  theme_fivethirtyeight()
```

![](OverExpectationModelsWithXGBoostMD_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

As we can see, Tua Tagovailoa for the Miami Dolphins is the only player
who has actually received above average help from his running game.
Burrow and Herbert have both been let down by their running games, but
Herbert more so. They say a running game is a Quarterbacks best friend,
and in this case, Tua is the only one benefitting from that.

## Yards After the Catch Over Expected

Using a near identical process as above, we can perform the same
analysis on a teams average yards after the catch over expectation.
Essentially, how much are a quarterbacks receivers helping him gain more
yardage after they have caught the ball.
![](OverExpectationModelsWithXGBoostMD_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Well, while Tua may be getting a lot of help from his running game, the
same can not be said about his receiving core, at least not on average
since 2020 (it would be interesting to see these numbers for just this
year). Also, Herbert is the only Quarterback of the three to be in the
negative for both metrics, and when you average the two metrics, his is
the lowest.

## Conclusion

In conclusion, there is not a clear winner or loser in terms of
supporting cast support as they are all fairly close on average, but
Herbert is technically the lowest overall. There is however a clear
winner for RYOE and loser for YACOE with Miami taking both.
