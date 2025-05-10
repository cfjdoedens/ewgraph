
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ewgraph

<!-- badges: start -->

<!-- badges: end -->

The goal of ewgraph is to represent and manipulate chance graphs as a
finite grid of data elements. The individual elements are called
segments. The grid is formed as a tibble. Each segment is a row of the
tibble.

The package design is not based on information hiding: the idea is that
users can know the details of how the chance graph is represented. And
possibly use this knowledge in unforeseen ways.

## Installation

You can install the development version of ewgraph like so:

``` r
if (file.exists("/home/crist-jan/R/x86_64-pc-linux-gnu-library/4.5/ewgraph")) {
  # We are executing on the author machine, use the development version available there. 
  loadNamespace("ewgraph")
} else {
  # Use the github version.
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  pak::pak("cfjdoedens/ewgraph")
}
#> <environment: namespace:ewgraph>
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ewgraph)
# Construct an ewgraph of 1000 segments, resulting from a binomial draw of 300 samples,
# with ten errors found.
g <- ew_from_vec(dbinom(x = 10, size = 300, partition_0_1(1000)))

# Get most probable value for the error fraction, p, and accompanying 
# probability density, h.
most_prob_ph <- ew_maxh(g)
print(most_prob_ph)
#>        h        p 
#> 38.29659  0.03350

# Get maximum value for the error fraction, p, given
# certainty level cert.
max_error_rate <- ew_maxcumh_p(g, .95)
print(max_error_rate)
#> [1] 0.05571198
```

## equal width probability graphs

### Introduction

An equal width probability graph, ewgraph, is a finite numeric
representation of a function c from domain P to codomain H.
$$c: P \rightarrow H$$ $P$ are the real numbers in $[0, 1]$. $H$ are the
real numbers in $[0,inf]$. $c$ is interpreted in this context as a
function that maps from an error fraction $p$ to the chance density of
that error fraction. $$h = c(p)$$ So $c$ is an absolutely continuous
probability distribution. And $c$ is a probability density function.

This function, $c$, is represented in an ewgraph as a finite set of $S$
pairs $(p_i, h_i)$, $i$ in $1:S$. $S$ a positive integer. $p_i$ are
elements of P, so of \[0, 1\]. $h_i$ are elements of H, so of \[0,
inf\]. $$h_i = c(p_i)$$

Let $$U = 1/(2*S)$$ We define the ith segment of $P$ as
$[i/S - 1/S, i/S]$. So all segments have equal width, $1/S = 2*U$. And
there are $S$ segments. The middle of the ith segment being
$$p_i = i/(S - U)$$ So the segments are:
$$[0, 1/S], [1/S, 2/S], ..., [(S - 1)/S, 1]$$

### The surface of a segment

An important aspect of a segment is its surface. For a probability
density function, the probability that an outcome lies on a certain
segment of P is proportional to the surface of that segment. We
approximate the surface of the probability function as the surfaces of
the segments of the corresponding equal width probability graph. The sum
of the surfaces of all segments is by definition 1.

As a first approximation we can equal the surface of a segment to
$h_i * (1/S)$. See drawing below of segment i

``` r
#                  |hi
#                  |
#                  |
#                  |
#                  |
#   _______________|________________
#   pi - U         pi               pi + U
#   =              =                =
#   i/S - 2U       i/S - U          i/S
```

$$surface_i = length\_of\_segment * height\_of\_segment$$
$$          = (p_i + U - (p_i - U))) * h_i$$ $$= 2U * h_i$$
$$= (1/S) * h_i$$

When the probability graph is rather flat this is a nice approximation.
However on steep parts of the graph, the approximation is less a good
fit. To improve on this we approximate the surface of a segment by
computing also the height of the begin and end points of the segment. So
by computing $h_{p_{i-U}} \equiv hleft_i$, and
$h_{p_{i+U}} \equiv hright_i$. We can then compute the surface of
segment $i$ as the sum of its left and right parts.

``` r
#                  |hi
#                  |
#                  |               |hrighti
#  |hlefti         |               |
#  |               |               |
#  |_______________|_______________|
#  pi - U          pi              pi + U
#   =              =                 =
# i/S - 2U        i/S - U            i/S
```

$$surface_i = surface\_left_i + surface\_right_i$$
$$surface\_left_i  = length\_of\_left\_halve\_of\_segment_i *
                   (hleft_i + h_i)/2$$ $$= U*(hleft_i + h_i)/2$$
$$surface\_right_i = length\_of\_right\_halve\_of\_segment_i *
                  (h_i + hright_i)/2$$ $$= U*(h_i + hright_i)/2$$
$$surface_i = U*(hleft_i + h_i)/2 + U*(h_i + hright_i)/2$$
$$= (hleft_i + hright_i + 2*h_i) * U /2$$

We compute $hleft_i$ and $hright_i$ by interpolation.

For $hleft_i$ we proceed: Let $$h_{im1} = c(p_{im1})$$ where $im1$
stands for $i - 1$. We then interpolate:
$$hleft_i  = (h_i + h_{im1})/2$$

``` r
#                                                  |hi
#                                  |hlefti         |
#                  |him1           |               |
#                  |               |               |
#   _______________|_______________|_______________|_______________
#                  pim1            pi - U          pi
#  segment i-1                     segment i
```

In the same vein we get $$hright_i = (h_i + h_{ip1})/2$$

``` r
#                  |hi
#                  |               |hrighti
#                  | hi            |               |hip1
#                  |               |               |
#   _______________|_______________|_______________|_______________
#                  pi              pim1 - U        pi
#  segment i                       segment i+1
```

This leaves us with the problem how to interpolate the leftmost
$hleft_i$, and the rightmost $hright_i$, i.e. $hleft_1$ and $hright_S$.
This because, we can not interpolate with hi from segment 0 or from
segment $S+1$, as these segments do not exist. For those cases instead
of interpolating we extrapolate.

For $hleft_1$:

``` r
#                                                 |h2
#                                 |               |
#                 |h1             |               |
#   |hleft1       |               |               |
#   |_____________|_______________|_______________|_______________
#   0             p1              p2 - U          p2
#   segment 1                     segment 2
```

The line through the coordinates $(U, h_1)$ and $(3*U, h_2)$ has the
algebraic form $y = ax+b$. So $$h_1 = a*U + b$$ $$h_2 = a*3*U + b$$ Then
we get $$a = (h_2 - h_1)/(2*U)$$ $$b = (3*h_1 - h_2)/2$$ So for
$hleft_1$ we get $$hleft_1 = a*0 + b$$ $$= b$$ $$= (3*h_1 - h_2)/2$$
However, note that $hleft_1$ could become less than 0. This we should
forbid, as it would imply a negative probability density.

So, taking this into account we get:
$$hleft_1 = max(0, (3*h_1 - h_2)/2)$$

For $hright_S$:

``` r
#                 |hSm1
#                 |               |
#                 | h1            |               |hS
#                 |               |               |              |hrightS
#    _____________|_______________|_______________|______________|
#                 pSm1            pS - U          pS             1
#    segment S-1                  segment S
```

The line through the coordinates $(1 - 3*U, h_{Sm1})$ and $(1 - U, h_S)$
has the algebraic form $$y = ax+b$$ So $$h_{Sm1} = a*(1-3*U) + b$$
$$h_S   = a*(1-U) + b$$ Then we get $$a = (h_S - h_{Sm1})/(2*U)$$
$$b = h_S + (h_{Sm1}-h_S)*(1-U)/(2*U)$$

So for $hright_S$ we get $$hright_S = a*1 + b$$ $$= a + b$$
$$= (h_S - h_{Sm1})/(2*U) + h_S + (h_{Sm1}-h_S)*(1-U)/(2*U)$$
$$= (3*h_S - h_{Sm1})/2$$ This outcome is symmetric with
$$hleft_1 = (3*h_1 - h_2)/2$$

As with $hleft_1$ we should forbid negative values for $hright_S$. So,
taking this into account we get:
$$hright_S = max(0, (3*h_S - h_{Sm1})/2)$$

### Representation of a segment from an ewgraph as a geometric figure

See the figure below. A segment of an ewgraph ABDEF can be considered to
consist of two rectangular trapeziums, ABCF and EDCF, representing
respectively the left and right halve of the segment. The corners ABC
and CDE are rectangular.

The two trapeziums have a shared parallel line, CF, which has length hi.
The three (one from the left trapezium, AB, one shared, CF, and one from
the right trapezium, DE) parallel lines are perpendicular to the p-axis,
BD. The lines of each trapezium BC and DC, that lie on the p-axis have
each length U. Line AB has length $hleft_i$. Line ED has length
$hright_i$.

``` r
#                  F
#                  "
#                 / \
#                / | \
#               /  |  \
#              /   |   \
#             /    |    \
#            /     |     \
#           /      |      \
#          /       |       \
#         /        |        \
#        /         |         \
#       /          |          \
#      /           |           \
#     /            |            \
#    /             |             \
#   /              |              \
#A |               |               \
#  |               |                \ E
#  |               |                |
#  |               |                |
#  |               |                |
#  |_______________|________________|
#  B               C                D
#
#  Figure: depiction of a segment from an ewgraph.
```

### Comparison with standard numerical approximation of an integral

I cite from Wikipedia:

> In calculus, the definite integral of an arbitrary function f(x) can
> be numerically approximated as a discrete sum by partitioning the
> interval of integration into small uniform intervals and approximating
> the function’s value on each interval as the average of the values at
> its endpoints.

We see here the correspondence between the small uniform intervals used
for calculating the definite integral and the segments of the ewgraph.

In contrast to the cited method an ewgraph is in essence based on the
middle value of a segment. The reason for this choice is that typically
a probability graph in the realm we are dealing with, might have an
infinite height at $p = 0$ or at $p = 1$. By steering clear from these
values, we avoid this problem. Furthermore I think that using the one
middle value of a segment as representative is more elegant and more
symmetric than using the values of the end points as representative. But
this is also a matter of taste.

### Representation of ewgraph as R tibble

In the programming language R we represent the ewgraph as a tibble with
S rows. Each ith segment of the ewgraph has the following variables:

- `p[[i]] = i/S - U`, this means that `p[[i]]` is the mid point of the
  ith segment
- `h[[i]] = c(p[[i]])` so `h[[i]]` is the chance of `p[[i]]`
- `h_left[[i]]` which is a linear interpolation of `h[[i]]` and
  `h[[i-1]]` (but a linear extrapolation of `h[[1]]` and `h[[2]]` for
  `h_left[[1]]`)
- `h_right[[i]]` which is a linear interpolation of `h[[i]]` and
  `h[[i+1]]` (but a linear extrapolation of `h[[S]]` and `h[[S-1]]` for
  `h_right(S)`)
- `surface[[i]]` which is the surface of this ith segment.
- `cumsurface[[i]]` which is the cumulative of `surface[[i]]`, going
  from 1 to i
