
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

# 2

## a

``` r
d <- readLines('data/2-input.txt')
direction <- sapply(strsplit(d, " "), `[[`, 1)
magnitude <- as.numeric(sapply(strsplit(d, " "), `[[`, 2))

horizontal <- sum(magnitude[direction == "forward"])
depth <- sum(magnitude[direction == "down"]) + sum(-magnitude[direction == "up"])
horizontal * depth
#> [1] 2120749
```

## b

``` r
l <- length(d) + 1
aim <- rep(0, l)
depth <- rep(0, l)
horiz <- rep(0, l)

for(i in seq_along(d)){
  if(direction[i] == "down"){
    aim[i+1] <- aim[i] + magnitude[i]
    depth[i+1] <- depth[i]
    horiz[i+1] <- horiz[i]
  } else if(direction[i] == "up"){
    aim[i+1] <- aim[i] - magnitude[i]
    depth[i+1] <- depth[i]
    horiz[i+1] <- horiz[i]
  } else if(direction[i] == "forward"){
    horiz[i+1] <- horiz[i] + magnitude[i]
    depth[i+1] <- depth[i] + (aim[i] * magnitude[i])
    aim[i+1] <- aim[i]
  }
}

horiz[l] * depth[l]
#> [1] 2138382217
```
