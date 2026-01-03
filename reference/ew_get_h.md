# Get the h column of an equal width graph.

Get the h column of an equal width graph.

## Usage

``` r
ew_get_h(g)
```

## Arguments

- g:

  The equal width graph

## Value

The h column as a vector

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_h(g)
#> [1] 0.5 1.0 1.5
```
