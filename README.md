
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Homework6

<!-- badges: start -->

[![R-CMD-check](https://github.com/akash-rajeev/Homework6/workflows/R-CMD-check/badge.svg)](https://github.com/akash-rajeev/Homework6/actions)
<!-- badges: end -->

The goal of Homework6 is to create a package that allows for basic
operations on sparse matrices. Contains several methods relating to the
development of sparse matrices, including arithmetic operations,
statistical operations, and display operations.

## Installation

You can install the development version of Homework6 in github like so:

``` r
# install.packages("devtools")
devtools::install_github("akash-rajeev/Homework6)
```

## Example

Here are some basic examples relating to the package

``` r
library(Homework6)

# Create a regular numeric vector
regular_vec = c(0, 1.5, 0, 0, 2.3, 0, 0, 4.1, 0, 0)

# Convert to sparse format
sparse_vec = as(regular_vec, "sparse_numeric")
print(sparse_vec)
#> Sparse Numeric Vector (length = 10 )
#> Non-zero values:
#>   position value
#> 1        2   1.5
#> 2        5   2.3
#> 3        8   4.1

# Create another sparse vector
regular_vec2 = c(0, 0.5, 0, 1.0, 2.3, 0, 0, 0, 3.2, 0)
sparse_vec2 = as(regular_vec2, "sparse_numeric")

# Arithmetic operations
sum_vec = sparse_vec + sparse_vec2
print(sum_vec)
#> Sparse Numeric Vector (length = 10 )
#> Non-zero values:
#>   position value
#> 1        2   2.0
#> 2        4   1.0
#> 3        5   4.6
#> 4        8   4.1
#> 5        9   3.2
diff_vec = sparse_vec - sparse_vec2
print(diff_vec)
#> Sparse Numeric Vector (length = 10 )
#> Non-zero values:
#>   position value
#> 1        2   1.0
#> 2        4  -1.0
#> 3        8   4.1
#> 4        9  -3.2
```
