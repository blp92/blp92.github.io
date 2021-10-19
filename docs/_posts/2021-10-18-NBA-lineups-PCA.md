---
layout: single
title:  "NBA Lineups Analysis"
header:
  teaser: "unsplash-gallery-image-2-th.jpg"
categories: 
  - Jekyll
tags:
  - edge case
---
================

## Loading in the libraries

``` r
library(tidyverse)
library(jsonlite)
library(ggplot2)
```

Next we need to actually import some data. This is from the nba.com
lineups stat page \#\# Importing data

``` r
nba_lineups_test <- fromJSON("~/Documents/Analyses/nba_lineups_response", flatten=F)

NBA_lineups_20_21 <- nba_lineups_test$resultSets$rowSet %>% .[[1]] %>% as_tibble()

colnames(NBA_lineups_20_21) <- nba_lineups_test$resultSets$headers %>% .[[1]]

NBA_lineups_20_21[,6:49] <- lapply(NBA_lineups_20_21[,6:49], as.numeric)
```

Now that we have the data into a tibble, we can run a principle
component analysis. PCA is a method for variable selection. Each
principle component is a linear combination of the n variables in an
m\*n matrix. The principle components are created so that each
successive principal component accounts for a smaller proportion of the
explained variance (i.e. PC1 accounts for the most, PC2 the second,
etc.).

## Running the PCA

``` r
NBA_lineups_20_21_pc <- prcomp(NBA_lineups_20_21[,6:49],
                               center=TRUE,
                               scale. = TRUE)
```

Note the

``` r
scale. = TRUE 
```

argument here scales the data to all be unit vector. This is important
in data sets where scales vary from variable to variable (note some are
percentagesin our data set, some are ratios, some are gross aggregates)
and without such a step, the standard deviatio of each component would
be exceptionally large, providing little predictive value if used in
inferential applications.

## Inital impression of PCA

By running summary(NBA\_lineups\_20\_21\_pc) we can glean some cursory
information.

``` r
summary(NBA_lineups_20_21_pc)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     3.4702 2.2937 2.2725 1.97432 1.82776 1.68750 1.64707
    ## Proportion of Variance 0.2737 0.1196 0.1174 0.08859 0.07593 0.06472 0.06166
    ## Cumulative Proportion  0.2737 0.3933 0.5106 0.59922 0.67515 0.73986 0.80152
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.55357 1.36831 1.14160 0.79069 0.61244 0.57965 0.48338
    ## Proportion of Variance 0.05485 0.04255 0.02962 0.01421 0.00852 0.00764 0.00531
    ## Cumulative Proportion  0.85637 0.89892 0.92854 0.94275 0.95128 0.95891 0.96422
    ##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
    ## Standard deviation     0.47340 0.46693 0.42260 0.37248 0.36233 0.34023 0.33271
    ## Proportion of Variance 0.00509 0.00496 0.00406 0.00315 0.00298 0.00263 0.00252
    ## Cumulative Proportion  0.96932 0.97427 0.97833 0.98148 0.98447 0.98710 0.98962
    ##                           PC22    PC23    PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     0.32080 0.24263 0.19579 0.18593 0.17663 0.17039 0.16372
    ## Proportion of Variance 0.00234 0.00134 0.00087 0.00079 0.00071 0.00066 0.00061
    ## Cumulative Proportion  0.99195 0.99329 0.99416 0.99495 0.99566 0.99632 0.99693
    ##                           PC29    PC30    PC31    PC32    PC33    PC34    PC35
    ## Standard deviation     0.15793 0.14921 0.13358 0.12029 0.11195 0.11086 0.10104
    ## Proportion of Variance 0.00057 0.00051 0.00041 0.00033 0.00028 0.00028 0.00023
    ## Cumulative Proportion  0.99749 0.99800 0.99841 0.99873 0.99902 0.99930 0.99953
    ##                           PC36    PC37    PC38    PC39    PC40     PC41
    ## Standard deviation     0.09266 0.08252 0.05967 0.03389 0.02349 0.001331
    ## Proportion of Variance 0.00020 0.00015 0.00008 0.00003 0.00001 0.000000
    ## Cumulative Proportion  0.99973 0.99988 0.99996 0.99999 1.00000 1.000000
    ##                            PC42      PC43      PC44
    ## Standard deviation     0.001187 0.0005008 1.203e-15
    ## Proportion of Variance 0.000000 0.0000000 0.000e+00
    ## Cumulative Proportion  1.000000 1.0000000 1.000e+00

In the world of stats rarely are arbitrary decisions made. That being
said, there are no rigid or mathematically sound heuristics for choosing
the number of principal components to incorporate into a model. Recall
that the goal here is for a re-dimensionalization, ideally to reduce the
number of variables from 44 to a handful that explain a large proportion
of the samples overall variance. We can see that only the top 11
principal components account for more than 1% of variation.

We may also plot a simple scree graph to see trends in the explained
variation as the number of components increases.

``` r
qplot(c(1:44), (NBA_lineups_20_21_pc$sdev^2/sum(NBA_lineups_20_21_pc$sdev^2))) +
        geom_line() +
        xlab("Principal Component") +
        ylab("Variance Explained") +
        ggtitle("Scree Plot For NBA Lineups Advanced Stats 2020-2021 Season") +
        ylim(0,1)
```

![](NBA_lineups_PCA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

