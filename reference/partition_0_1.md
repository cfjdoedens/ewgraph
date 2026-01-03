# Partition `[0, 1]` into equally sized consecutive segments

Each segment is represented by its midpoint.

## Usage

``` r
partition_0_1(S = 1000)
```

## Arguments

- S:

  An integer \>= 1. The number of segments to partition into.

## Value

The vector of segment midpoints.

## Examples

``` r
  # Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
  partition_0_1(S = 5)
#> [1] 0.1 0.3 0.5 0.7 0.9
```
