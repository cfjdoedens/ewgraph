# Add surface and cumulative surface to segments of equal width graph.

This also means adapting the columns h, h_left and h_right in order to
normalize the total chance mass to 1.

## Usage

``` r
ew_add_surface(g)
```

## Arguments

- g:

  should be an equal width graph, represented as a tibble with vectors p
  and h, h_left and h_right.

  For a description of equal width probability graphs see
  ew_minmaxcumh_p.R.

## Value

The augmented graph as a tibble.

Examples g \<- ew_add_surface(g)
