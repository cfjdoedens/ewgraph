# minimum error rate for given certainty

Returns the highest (possibly interpolated) error rate, p, for which the
accumulation of the chance density, h, starting from the last element of
`g` and going to the first element of `g` is `>= cert`.

## Usage

``` r
ew_mincumh_p(g, cert = 0.95)
```

## Arguments

- g:

  An ewgraph.

- cert:

  The certainty. Lies between 0 and 1.

## Value

The minimum error rate of `g`, given certainty `cert`.

## Examples

``` r
  S <- 1000
  g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
  min_cumh_p <- ew_mincumh_p(g)
  print(min_cumh_p)
#> [1] 0.07552868
```
