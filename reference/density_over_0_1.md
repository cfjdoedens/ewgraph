# Create vector of densities over `[0, 1]`

Returns a vector of counts of the input. The count is per i in 1:S,
where the elements of the input in `(i/S - 1/2S, i/S + 1/2S]` are
counted in the same bucket.

## Usage

``` r
density_over_0_1(values_from_0_1, S)
```

## Arguments

- values_from_0_1:

  Is a vector of values from `[0, 1]`

- S:

  The number of buckets in which we separate the elements of the input
  vector.

## Value

A vector of size `S`. Each element i of the vector contains the count of
the values from `values_from_01` that have a value in the range (1/S -
1/2S, 1/S + 1/2S\].

## Details

The values equal 0 are counted in the first bucket.

## Examples

``` r
    y <- seq(0, 1, length.out=33)
    x <- density_over_0_1(values_from_0_1 = y, S = 12)
```
