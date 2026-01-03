# Return TRUE iff the highest value of h is reached neither for the h_left of the first segment nor for the h_right of the last segment

Return TRUE iff the highest value of h is reached neither for the h_left
of the first segment nor for the h_right of the last segment

## Usage

``` r
ew_middle_peaked(g)
```

## Arguments

- g:

  An ewgraph.

## Value

TRUE or FALSE.

## Examples

``` r
  S <- 1000
  g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
  middle_peaked <- ew_middle_peaked(g)
  print(middle_peaked)
#> [1] TRUE
```
