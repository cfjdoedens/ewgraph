# Checks if a function is monotone rising over a specified interval.

Checks if a function is monotone rising over a specified interval.

## Usage

``` r
is_monotone_rising(f, range = c(0, 1), n_points = 1000, strictly = FALSE)
```

## Arguments

- f:

  The function to test. It should be a function that takes a numeric
  vector and returns a numeric vector.

- range:

  A numeric vector of length 2, defining the start and end of the
  interval to check (e.g., c(0, 100)).

- n_points:

  The number of points to sample within the range. The more points, the
  more thorough the check.

- strictly:

  A boolean. If TRUE, checks for strictly increasing (f(x2) \> f(x1)).
  If FALSE (default), checks for non-decreasing (f(x2) \>= f(x1)).

## Value

TRUE if the function is monotone rising on the sampled points, FALSE
otherwise.

## Examples

``` r
  f_linear_large <- function(x) 2 * x + 5
  should_be_true <- is_monotone_rising(f_linear_large,
                                       range = c(-1e6, 1e6),
                                       n_points = 1000)
```
