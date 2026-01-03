# Return the (possibly interpolated) highest value of h, h_left, or h_right and corresponding error rate, p.

Return as a named list of the form c(h = h_highest, p = p_highest).

## Usage

``` r
ew_maxh(g)
```

## Arguments

- g:

  An ewgraph.

## Value

c(h = h_highest, p = p_highest)

## Examples

``` r
  S <- 1000
  g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
  hp_max <- ew_maxh(g)
  print(hp_max)
#>        h        p 
#> 23.03533  0.10050 
```
