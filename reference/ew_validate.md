# Check whether object is a valid equal width graph

Check whether object is a valid equal width graph

## Usage

``` r
ew_validate(g, verbose = TRUE)
```

## Arguments

- g:

  The object to be checked

- verbose:

  TRUE or FALSE

## Value

TRUE or FALSE When g is invalid as an equal width graph and verbose ==
TRUE, info why g is invalid is printed to the console.

## Examples

``` r
  g <- ew_validate(c(1, 2, 3))
#> ew_probability_graph should be a tibble
  ew_validate(g)
#> ew_probability_graph should be a tibble
#> [1] FALSE
```
