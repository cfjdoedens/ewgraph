# Get the h_right column of an equal width graph.

Get the h_right column of an equal width graph.

## Usage

``` r
ew_get_h_right(g)
```

## Arguments

- g:

  The equal width graph

## Value

The h_right column as a vector

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_h_right(g)
#> [1] 0.75 1.25 1.75
```
