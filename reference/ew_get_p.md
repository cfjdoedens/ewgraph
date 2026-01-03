# Get the p column of an equal width graph.

Get the p column of an equal width graph.

## Usage

``` r
ew_get_p(g)
```

## Arguments

- g:

  The equal width graph

## Value

The p column as a vector

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_p(g)
#> [1] 0.1666667 0.5000000 0.8333333
```
