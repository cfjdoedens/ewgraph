# Create an equal width probability graph from vector `v`

We assume that the elements of the vector represent the probabilities p,
of 1/S - 1/2S, ..., S/S - 1/2S. `S` is the number of elements, so the
length, of the vector. We call the corresponding elements of the equal
width probability graph *segments* of the probability graph.

## Usage

``` r
ew_from_vec(v)
```

## Arguments

- v:

  The input vector.

## Value

An equal width chance graph representation of the given vector.
