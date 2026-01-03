# Get the number of segments, i.e. number of rows, of an equal width graph.

Get the number of segments, i.e. number of rows, of an equal width
graph.

## Usage

``` r
ew_S(g)
```

## Arguments

- g:

  The equal width graph

## Value

The number of segments, this is, rows of g

## Examples

``` r
  g <- ew_from_vec(c(1, 2, 3))
  ew_S(g)
#> [1] 3
```
