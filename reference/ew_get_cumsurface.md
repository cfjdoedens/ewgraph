# Get the cumulative surface column of an equal width graph.

Get the cumulative surface column of an equal width graph.

## Usage

``` r
ew_get_cumsurface(g)
```

## Arguments

- g:

  The equal width graph

## Value

The cumulative surface column as a vector

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_cumsurface(g)
#> [1] 0.1666667 0.5000000 1.0000000
```
