# highest part of the graph with a given cumulative chance density

highest part of the graph with a given cumulative chance density

## Usage

``` r
ew_high_density_interval(g, high_density_area = 0.999)
```

## Arguments

- g:

  An ewgraph.

- high_density_area:

  When 1 return `g` as is. When smaller than 1, the highest part of the
  ewgraph `g` will be shown which has a cumulative chance density equal
  to `high_density_interval` The idea is that this cuts away the
  uninteresting low value left and/or right part of the graph, and
  leaves only the rest.

## Value

A vector of the form `c(left = i_left, right = i_right)`. Here `i_left`
and `i_right` are respectively the leftmost and rightmost ew segment of
the HDI.

## Examples

``` r
  S <- 1000
  g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
  lr  <- ew_high_density_interval(g)
  print(lr)
#>  left right 
#>    54   166 
```
