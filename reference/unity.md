# One argument constant function that returns 1

The domain of this function is the numeric values. Other input values
will result in an error.

## Usage

``` r
unity(x)
```

## Arguments

- x:

  Should be numeric.

## Value

1 for x being numeric, otherwise an error.

## Examples

``` r
  # Returns 1.
  unity(-0.034)
#> [1] 1

  # Returns 1.
  unity(rnorm(1, mean = 10^10, sd = 10^20))
#> [1] 1

  # Returns 1.
  unity(NaN)
#> [1] 1

  # Returns 1.
  unity(Inf)
#> [1] 1

  # Returns c(1, 1).
  unity(c(3, 7))
#> [1] 1 1

  # An error is generated for non-numeric input.
  tryCatch({
    unity("a")
  }, error = function(e) {
    cat('unity("a") procuded the following error message:', conditionMessage(e), "\n")
    NA
  })
#> unity("a") procuded the following error message: is.numeric(x) is not TRUE 
#> [1] NA
```
