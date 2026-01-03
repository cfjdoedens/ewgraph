# Get the surface column of an equal width graph.

Get the surface column of an equal width graph.

## Usage

``` r
ew_get_surface(g)
```

## Arguments

- g:

  The equal width graph

## Value

The surface column as a vector

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_surface(g)
#> [1] 0.1666667 0.3333333 0.5000000
```
