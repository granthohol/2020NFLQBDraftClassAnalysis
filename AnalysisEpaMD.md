Analysis of EPA
================
Grant Hohol

## Career EPA Analysis

EPA: Expected Points Added - it is the number one metric in determining
the true success of a play.

Analyzing each Quarterbacks EPA over the course of their career.

Load necessary packages

``` r
library(ggthemes)
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
library(ggimage)
```

## Data Manipulation

Load play by play data for seasons 2020-2023 and create a data frame of
only pass plays from that seasons

``` r
# makes it quicker to load in multiple years of play by play data
future::plan("multisession") 
pbp <- load_pbp(2020:2023)

pbp_passes <- pbp %>%
  filter(season_type == "REG") %>%
  filter(qb_dropback == 1) %>%
  # get rid of mahomes for scaling reasons
  filter(!(passer == "P.Mahomes")) %>%  
  filter(!is.na(epa)) %>%
  group_by(passer) %>%
  mutate(total_passes = n()) %>%
  ungroup() %>%
  filter(total_passes >= 200)
```

Create columns in the data set for dropback number and cumulative epa

``` r
# ave(X, GROUP, FUN)
# X: This is the variable for which you want to calculate the summary statistic (e.g., mean, sum, median).
# GROUP: This is the grouping variable, which defines the groups based on which you want to compute the summary statistic.
# FUN: This is the function that specifies the summary statistic you want to calculate, such as mean, sum, median, or any other applicable function.
pbp_passes$dropback_num <- ave(pbp_passes$epa, pbp_passes$passer, FUN = seq_along) # creating a dropback number column 
pbp_passes$csum <- ave(pbp_passes$epa, pbp_passes$passer, FUN = cumsum) # create a cumulative epa sum column
```

Create data frame for each QB we care about

``` r
tua_stats <- pbp_passes %>%
  filter(passer == "T.Tagovailoa")
herbert_stats <- pbp_passes %>%
  filter(passer == "J.Herbert")
burrow_stats <- pbp_passes %>%
  filter(passer == "J.Burrow")
```

## Graphing

Creating the graph

``` r
teams_colors_logos %>%
  filter(team_abbr %in% c("MIA", "LAC", "CIN")) %>%
  select(team_abbr, team_color)
```

    ## # A tibble: 3 × 2
    ##   team_abbr team_color
    ##   <chr>     <chr>     
    ## 1 CIN       #FB4F14   
    ## 2 LAC       #007BC7   
    ## 3 MIA       #008E97

``` r
pbp_passes %>%
  ggplot(aes(x = dropback_num, y = csum)) + 
  geom_smooth(aes(group = passer), color = "gray", se= FALSE, linewidth = 1.5) + # graph every QB besides the chosen 3
  geom_smooth(data = tua_stats, aes(group = passer), color = "#008E97", se = FALSE, linewidth = 3) + # line for tua 
  geom_smooth(data = herbert_stats, aes(group = passer), color = "#007BC7", se = FALSE, linewidth = 3) + # line for herbert
  geom_smooth(data = burrow_stats, aes(group = passer), color = "#FB4F14", se = FALSE, linewidth = 3) +  # line for burrow
  labs(x = "Dropback Number",
       y = "Cumulative EPA",
       title = "Cumulative Dropback EPA Over Career") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(size = 14)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) + 
  theme_fivethirtyeight() 
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](AnalysisEpaMD_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

As we can see, at the start of their careers it was fairly clearly
Herbert\>Burrow\>Tua. Then, in their mid careers Burrow and Tua are
about tied with Herbert slightly ahead, but due to recent play, Burrow
has risen to Herbert’s level while Tua is still slightly behind.

## Create Career Average EPA Graph

Create new data frame of passers with a condition for our three
Quarterbacks for graphing reasons

``` r
passers <- pbp_passes %>%
  group_by(passer) %>%
  summarize(passes = n(), avg_epa = mean(epa), team_abbr = last(posteam)) %>%
  arrange(-passes) %>%
  slice_head(n = 32) %>% # only have the top 32 Qbs in the table
  filter(!(passer=="Z.Wilson")) %>%
  left_join(teams_colors_logos, by = "team_abbr") 

# Create a new data frame "passers_combined" by adding a "condition" column based on the "passer" values.
# If "passer" is one of the specified players, set "condition" to "Selected," otherwise, set it to "Other."  
passers_combined <- passers %>%
  mutate(condition = ifelse(passer %in% c("T.Tagovailoa", "J.Herbert", "J.Burrow"), "Selected", "Other"))
```

Make the graph

``` r
# Create a ggplot plot using the "passers_combined" data frame.
passers_combined %>%
  ggplot(aes(x = passes, y = avg_epa, fill = condition, color = condition)) +
  # Add points to the plot with specified shape (shape 21, which is a filled circle).
  geom_point(shape = 21) +
  # Set colors for the "Selected" and "Other" conditions.
  scale_color_manual(values = c("Selected" = "black", "Other" = "gray")) +
  scale_fill_manual(values = c("Selected" = "black", "Other" = "gray")) +
  geom_hline(yintercept = mean(passers$avg_epa), linetype = "dashed") + 
  geom_vline(xintercept = mean(passers$passes), linetype = "dashed") +
  # Add labels for each data point using the "ggrepel" package.
  ggrepel::geom_text_repel(aes(label = passer), size = 5) +
  labs(x = "Dropback Number", y = "Average EPA/Play",
       title = "Average EPA/Dropback for NFL Quarterbacks, 2020-2023",
       subtitle = "2020 Draft Class Highlighted") + 
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  theme_bw()
```

![](AnalysisEpaMD_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Despite the previous graph showing Tua being below Burrow and Herbert,
since he has so many less dropbacks, his slope is actually on pace to
surpass Burrow and Herbert by the time he gets to their number of
dropbacks. This is shown by his career average EPA/Play being higher
than Burrow and Herbert.

## Conclusion

Tua is technically on pace to surpass Herbert and Burrow, but we’ve seen
both Herbert and Burrows EPA/Play drop around dropback 1500 which Tua is
yet to reach. Only time will tell if Tua can avoid that drop. If he
does, he is a clear winner.
