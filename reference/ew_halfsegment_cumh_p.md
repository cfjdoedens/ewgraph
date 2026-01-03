# In ew half segment, given cumulative value for h, find corresponding p value

An ew half segment of an equal width graph, or ewgraph, is a rectangular
trapezium with baseline size U and two parallel sides perpendicular to
the base line.

## Usage

``` r
ew_halfsegment_cumh_p(h1, h2, sd, U)
```

## Arguments

- h1:

  The height of the left side of the trapezium.

- h2:

  The height of the right side of the trapezium.

- sd:

  Alias for surface_delta: The left part of the surface of the trapezium
  bordered by an imaginary line perpendicular to the base line of the
  trapezium. sd \> 0, and smaller than the surface of the trapezium.

- U:

  The length of the baseline of the trapezium.

## Value

The computed value of x, which is the point where the imaginary line
crosses the base line.

## Examples

``` r
  x <- ew_halfsegment_cumh_p(h1 = 1, h2 = 0, sd = 0.0125, U = 0.25)
```
