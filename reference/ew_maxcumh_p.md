# maximum error rate for given certainty

Returns the lowest (possibly interpolated) error rate, p, for which the
accumulation of the chance density, h, starting from the first element
of g and going to the last element of `g` is `>= cert`.

## Usage

``` r
ew_maxcumh_p(g, cert = 0.95)
```

## Arguments

- g:

  An ewgraph.

- cert:

  The certainty. Lies between 0 and 1.

## Value

The maximum error rate of `g`, given certainty `cert`.

## Examples

``` r
  S <- 1000
  g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
  max_cumh_p <- ew_maxcumh_p(g)
  print(max_cumh_p)
#> [1] 0.1327614
```
