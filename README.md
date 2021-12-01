
<!-- README.md is generated from README.Rmd. Please edit that file -->

# advent-of-code

Attempting [advent of code](https://adventofcode.com/) 2021 in base R.

I am a massive fan and regular user of the tidyverse, but every now and
then I like to remind myself of what life used to be like…

# 1

## a

``` r
d <- as.numeric(readLines('data/1-input.txt'))
sum(diff(d) > 0)
#> [1] 1167
```

## b

``` r
r <- sapply(1:(length(d)-length(d)%%3), function(x) sum(d[x:(x+2)]))
sum(diff(r) > 0)
#> [1] 1130
```
