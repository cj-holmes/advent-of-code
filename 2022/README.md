
<!-- README.md is generated from README.Rmd. Please edit that file -->

# advent-of-code

``` r
library(tidyverse)
library(here)
```

# — Day 1: Calorie Counting —

## 1a

``` r
d <- 
  readr::read_lines(here('2022', 'data-in', '1.txt')) |>
  as.numeric() |> 
  tibble(x = _) |> 
  mutate(g = case_when(is.na(x) ~ 1, TRUE ~ 0),
         g = cumsum(g)) |> 
  filter(!is.na(x))

d |> 
  group_by(g) |>
  summarise(calories = sum(x)) |>
  slice_max(calories, n=1)
#> # A tibble: 1 × 2
#>       g calories
#>   <dbl>    <dbl>
#> 1   183    67450
```

## 1b

``` r
d |> 
  group_by(g) |>
  summarise(calories = sum(x)) |>
  slice_max(calories, n=3) |> 
  summarise(sum(calories))
#> # A tibble: 1 × 1
#>   `sum(calories)`
#>             <dbl>
#> 1          199357
```

# — Day 2: Rock Paper Scissors —

## 2a

``` r
result_lu <- c(l = 0, d = 3, w = 6)
shape_lu <- c(X = 1, Y = 2, Z = 3)

d <- 
  tibble(raw = read_lines(here('2022', 'data-in', '2.txt'))) |> 
  separate(raw, into = c('p1', 'p2'), sep = ' ', remove = FALSE) |> 
  mutate(p2_result = 
           case_when(
             p1 == 'A' & p2 == 'X' ~ 'd',
             p1 == 'A' & p2 == 'Y' ~ 'w',
             p1 == 'A' & p2 == 'Z' ~ 'l',
             p1 == 'B' & p2 == 'X' ~ 'l',
             p1 == 'B' & p2 == 'Y' ~ 'd',
             p1 == 'B' & p2 == 'Z' ~ 'w',
             p1 == 'C' & p2 == 'X' ~ 'w',
             p1 == 'C' & p2 == 'Y' ~ 'l',
             p1 == 'C' & p2 == 'Z' ~ 'd'),
         shape_point = shape_lu[p2],
         result_point = result_lu[p2_result],
         score = shape_point + result_point)

sum(d$score)
#> [1] 11906

# Redefine result lookup
result_lu <- c(X = 0, Y = 3, Z = 6)

d <- 
  tibble(raw = read_lines(here('2022', 'data-in', '2.txt'))) |> 
  separate(raw, into = c('p1', 'p2_result'), sep = ' ', remove = FALSE) |> 
  mutate(p2 = 
           case_when(
             p1 == 'A' & p2_result == 'X' ~ 'Z',
             p1 == 'A' & p2_result == 'Y' ~ 'X',
             p1 == 'A' & p2_result == 'Z' ~ 'Y',
             p1 == 'B' & p2_result == 'X' ~ 'X',
             p1 == 'B' & p2_result == 'Y' ~ 'Y',
             p1 == 'B' & p2_result == 'Z' ~ 'Z',
             p1 == 'C' & p2_result == 'X' ~ 'Y',
             p1 == 'C' & p2_result == 'Y' ~ 'Z',
             p1 == 'C' & p2_result == 'Z' ~ 'X'),
         shape_point = shape_lu[p2],
         result_point = result_lu[p2_result],
         score = shape_point + result_point)

sum(d$score)
#> [1] 11186
```

# — Day 3: Rucksack Reorganization —

## 3a

``` r
priority_lu <- set_names(1:52, c(letters, LETTERS))

d <-
  read_lines(here('2022', 'data-in', '3.txt')) |> 
  tibble(raw = _) |> 
  mutate(nchar = nchar(raw),
         string = str_split(raw, ""),
         intersect = map2_chr(string, nchar, ~intersect(.x[1:(.y/2)], .x[((.y/2)+1):.y])),
         priority = priority_lu[intersect]) 

sum(d$priority)
#> [1] 8176
```

## 3b

``` r
d |> 
  mutate(g = rep(1:(nrow(d)/3), each = 3)) |> 
  group_by(g) |> 
  summarise(intersect = intersect(intersect(string[[1]], string[[2]]), string[[3]])) |> 
  mutate(priority = priority_lu[intersect]) |> 
  pull(priority) |> 
  sum()
#> [1] 2689
```

# — Day 4: Camp Cleanup —

## 4a

``` r
d <-
  read_lines(here('2022', 'data-in', '4.txt')) |> 
  tibble(raw = _) |> 
  separate(raw, into = c('e1', 'e2'), sep = ',') |> 
  separate(e1, into = c('e1_start', 'e1_end'), sep = '-', convert = TRUE) |> 
  separate(e2, into = c('e2_start', 'e2_end'), sep = '-', convert = TRUE) |> 
  mutate(
    e1 = map2(e1_start, e1_end, ~.x:.y),
    e2 = map2(e2_start, e2_end, ~.x:.y),
    intersect = map2(e1, e2, intersect)) |> 
  mutate(full_cover = lengths(intersect) == lengths(e1) | lengths(intersect) == lengths(e2))

sum(d$full_cover)
#> [1] 471
```

## 4b

``` r
d <- d |> mutate(any_cover = lengths(intersect) > 0)
sum(d$any_cover)  
#> [1] 888
```