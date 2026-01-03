# Round probability value in accordance with number of segments of equal width graph

This function needs some more thought. It now returns a string. It is
probably better if it returns a number. It needs also better explaining.

## Usage

``` r
ew_round_prob(p, S)
```

## Arguments

- p:

  The probability to round

- S:

  The number of segments of teh equal width graph

## Value

The rounded value

## Examples

``` r
  ew_round_prob(0.0356, 10)
#> [1] "0.0"
```
