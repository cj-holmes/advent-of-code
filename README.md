
<!-- README.md is generated from README.Rmd. Please edit that file -->

# advent-of-code

Attempting the 2021 [advent of code](https://adventofcode.com/) in
**base R** only!

I am a massive fan and regular user of the tidyverse, but every now and
then I like to remind myself of what life used to be like…

# 1 Sonar Sweep

## 1a

``` r
d <- as.numeric(readLines('data/1-input.txt'))
sum(diff(d) > 0)
#> [1] 1167
```

## 1b

``` r
r <- sapply(1:(length(d)-length(d)%%3), function(x) sum(d[x:(x+2)]))
sum(diff(r) > 0)
#> [1] 1130
```

# 2 Dive!

## 2a

``` r
d <- readLines('data/2-input.txt')
direction <- sapply(strsplit(d, " "), `[[`, 1)
magnitude <- as.numeric(sapply(strsplit(d, " "), `[[`, 2))

horizontal <- sum(magnitude[direction == "forward"])
depth <- sum(magnitude[direction == "down"]) + sum(-magnitude[direction == "up"])
horizontal * depth
#> [1] 2120749
```

## 2b

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

# 3 Binary Diagnostic

## 3a

``` r
d <- readLines('data/3-input.txt')
m <- matrix(as.integer(unlist(strsplit(d, ""))), ncol = 12, byrow = TRUE)

gamma <- strtoi(paste0(as.integer(colMeans(m) > 0.5), collapse=""), base=2)
epsilon <- strtoi(paste0(as.integer(colMeans(m) < 0.5), collapse=""), base=2)

gamma * epsilon
#> [1] 1307354
```

## 3b

``` r
# O2
mi <- m
for(i in 1:ncol(mi)){
  if(nrow(mi) == 1) break
  mi <- mi[which(mi[,i] == round(ifelse(mean(mi[,i]) == 0.5, 1, mean(mi[,i])))),,drop = F]}
o2 <- strtoi(paste0(as.vector(mi), collapse = ""), base=2)

# CO2
mi <- m
for(i in 1:ncol(mi)){
  if(nrow(mi) == 1) break
  mi <- mi[which(mi[,i] == !round(ifelse(mean(mi[,i]) == 0.5, 1, mean(mi[,i])))),,drop = F]}
co2 <- strtoi(paste0(as.vector(mi), collapse = ""), base=2)

o2 * co2
#> [1] 482500
```

# 4 Giant Squid

## 4a

``` r
d <- readLines('data/4-input.txt')

# Extract numbers
nums <- as.integer(unlist(strsplit(d[1], ",")))

# Extract bingo boards
# trimws() to remove leading spaces for single digit numbers
# gsub() all double spaces to single spaces so I can strsplit() on single spaces
boards <-
  lapply(
    lapply(which(d == ""), function(x) gsub("  ", " ", trimws(d[(x+1):(x+5)]))),
    function(x) matrix(as.integer(unlist(strsplit(paste0(x, combine=" "), " "))),
                       ncol=5, byrow=TRUE))
```

Function to return if a matrix has a completed column or row (where
completed numbers are NA)

``` r
is_bingo <- function(m){
  
  check_rows <- apply(m, MARGIN = 1, function(x) all(is.na(x)))
  check_cols <- apply(m, MARGIN = 2, function(x) all(is.na(x)))
  
  if(any(check_cols) | any(check_rows)){return(TRUE)} else {return(FALSE)}
}
```

``` r
boards_tmp <- boards

for(i in seq_along(nums)){
  
  # Update boards
  boards_tmp <- lapply(boards_tmp, function(x) replace(x, x==nums[i], values = NA))
  
  # Test for any with BINGO
  results <- sapply(boards_tmp, is_bingo)

  # If any board has BINGO, return results
  if(any(results)){
    print(paste0("BINGO!"))
    # hopefully only one board wins per number called...
    winning_board <- boards_tmp[[which(results)[1]]] 
    print(winning_board)
    winning_board_sum <- sum(winning_board[!is.na(winning_board)])
    number_called <- nums[i]
    break}
  }
#> [1] "BINGO!"
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   46   52   40   12   44
#> [2,]    0   73   20   86    1
#> [3,]   NA   NA    4   NA   NA
#> [4,]   NA   NA   56   39   NA
#> [5,]   NA   NA   NA   NA   NA

winning_board_sum * number_called
#> [1] 23177
```

## 4b

``` r
boards_tmp <- boards

for(i in seq_along(nums)){
  
  # Update boards
  boards_tmp <- lapply(boards_tmp, function(x) replace(x, x==nums[i], values = NA))
  
  # Test for any with BINGO
  results <- sapply(boards_tmp, is_bingo)
  
  # If only one board remaining to get BINGO - save the ID of the board
  # This will then run every time until the final board is solved
  if(sum(!results) == 1){
    print("One board left to get BINGO")
    last_board_id <- which(!results)
  }

  # When all boards are solved, return the final board (solved) and the final
  # number called
  if(all(results)){
    print("All boards have BINGO")
    final_board <- boards_tmp[[last_board_id]]
    final_number <- nums[i]
    break
  }
}
#> [1] "One board left to get BINGO"
#> [1] "One board left to get BINGO"
#> [1] "One board left to get BINGO"
#> [1] "One board left to get BINGO"
#> [1] "All boards have BINGO"

sum(final_board[!is.na(final_board)]) * final_number
#> [1] 6804
```
