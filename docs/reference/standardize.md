# Standardize Sparse Numeric Vector

Standardize sparse numeric vector, by subtracting mean and dividing by
standard deviation.

## Usage

``` r
standardize(x, ...)

# S4 method for class 'sparse_numeric'
standardize(x, na.rm = FALSE, ...)
```

## Arguments

- x:

  A sparse_numeric object

- ...:

  Additional arguments (not used)

- na.rm:

  logical, should missing values be removed?

## Value

standardized sparse_numeric object
