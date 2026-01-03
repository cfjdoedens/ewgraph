# Partition line piece on the real line into equally sized consecutive segments

Each segment is represented by its midpoint.

## Usage

``` r
partition(begin = 0, end = 1, S = 1000)
```

## Arguments

- begin:

  The start of the line piece.

- end:

  The end of the line piece.

- S:

  An integer \>= 1. The number of segments to partition into.

## Value

The vector of segment midpoints.

## Examples

``` r
  # Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
  partition(S = 5)
#> [1] 0.1 0.3 0.5 0.7 0.9
```
