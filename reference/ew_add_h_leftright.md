# Add left and right interpolated and extrapolated heights to segments of equal width graph.

For a description of equal width probability graphs see above.

## Usage

``` r
ew_add_h_leftright(g)
```

## Arguments

- g:

  should be an equal width graph, represented as a tibble with vectors p
  and h.

## Value

The augmented graph as a tibble.

Examples g \<- ew_add_h_leftright(g)
