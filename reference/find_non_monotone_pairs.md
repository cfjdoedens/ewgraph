# Find input and output pairs of a function that are not monotone rising.

Gather pairs of input and output in a would be monotone rising function
where the function is not monotone rising. This function is useful for
debugging purposes. Show the pairs of monotone rising inputs, with their
non rising outputs.

## Usage

``` r
find_non_monotone_pairs(f, in_vals, strictly = FALSE)
```

## Arguments

- f:

  The function to test. It should be a function that takes a numeric
  vector and returns a numeric vector.

- in_vals:

  Vector of numeric input values for f.

- strictly:

  A boolean. If TRUE, checks for strictly increasing (f(x2) \> f(x1)).
  If FALSE (default), checks for non-decreasing (f(x2) \>= f(x1)).

## Value

A data frame with columns `input1`, `output1`, `input2`, `output2`
containing the offending pairs of input and their outputs.

## Examples

``` r
# Example function that is not monotone rising
example_function <- function(x) {
  ifelse(x < 0.5, x, 1 - x)
}

# Find non-monotone pairs in the example function.
x <- find_non_monotone_pairs(example_function, seq(
  from = 0,
  to = 1,
  length.out = 100), strictly = FALSE)
```
