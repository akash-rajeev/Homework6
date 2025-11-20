## HW5 Class/Methods

# Sparse Vector Class

#' Sparse Numeric Vector Class
#'
#' S4 class that represents vectors as sparse vectors, storing only nonzero values and positions.
#'
#' @importFrom methods new as validObject setClass setValidity setGeneric setMethod show
#' @importFrom graphics points legend plot
#'
#' @slot value nonzero values vector
#' @slot pos nonzero value positions vector
#' @slot length integer representing length of vector
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

# Validity Function
setValidity("sparse_numeric", function(object) {

  # Check if value and pos same length
  if (length(object@value) != length(object@pos)) {
    return("@value and @pos must be same length")
  }
  # Check that length is valid
  if (object@length < 0 | length(object@length) != 1){
    return("@length must be greater than 0 and be a single value")
  }
  # Check that all positions are valid
  if (any(object@pos < 1)  || any(object@pos > object@length) ){
    return("positions are either less than 0 or greater than length of vector")
  }
  # Check that position values are unique
  if (length(unique(object@pos)) != length(object@pos)){
    return("position values are not unique")
  }
  # Check that all values in value are nonzero
  if (any(object@value == 0)){return("not all values in @value are nonzero")}
  # Check that matrix is sorted
  if (is.unsorted(object@pos)) {return("not sorted")}

  TRUE

})


# Addition Function
#' Add Two Sparse Numeric Vectors
#'
#' Adds two sparse vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param e1 sparse_numeric object
#' @param e2 sparse_numeric object
#' @param ... Additional arguments, not used
#' @return sparse_numeric object containing sum
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y, ...){
  # check that x and y have same length
  if (x@length != y@length){stop("vectors are not same length")}

  # find what positions to look at
  all_pos_add = sort(unique(c(x@pos, y@pos)))

  # calculate the sum at each position
  all_sums = c()
  all_positions_add = c()

  for (pos in all_pos_add){
    val = 0

    if (pos %in% x@pos){
      idx = which(x@pos == pos)
      val = val + x@value[idx]
    }

    if (pos %in% y@pos){
      idx = which(y@pos == pos)
      val = val + y@value[idx]
    }

    if (val != 0){
      all_sums = c(all_sums, val)
      all_positions_add = c(all_positions_add, pos)
    }
  }

  if(length(all_sums) == 0){
    all_sums = numeric(0)
    all_positions_add = integer(0)
  }

  # check if results have same length
  if(length(all_sums) != length(all_positions_add)){stop("result vectors not same length")}

  return(new("sparse_numeric", value = all_sums, pos = all_positions_add, length = x@length))
})

# Multiplication Function
#' Multiply Two Sparse Numeric Vectors
#'
#' multiplication of two sparse vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param e1 sparse_numeric  object
#' @param e2 sparse_numeric object
#' @param ... Additional arguments, not used
#' @return sparse_numeric object containing product
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  # check x y same length
  if (x@length != y@length){stop("vectors are not same length")}

  # find what positions to look at (common non-zero positions)
  all_pos_mult = intersect(x@pos, y@pos)
  product_vals = c()
  product_pos = c()

  for (pos in all_pos_mult){
    # get x value
    x_idx = which(x@pos == pos)
    x_val = x@value[x_idx]

    # get y value
    y_idx = which(y@pos == pos)
    y_val = y@value[y_idx]

    # multiply values
    product = x_val * y_val

    # only keep non-zero
    if (product != 0){
      product_vals = c(product_vals, product)
      product_pos = c(product_pos, pos)
    }
  }

  if(length(product_vals) == 0){
    product_vals = numeric(0)
    product_pos = integer(0)
  }

  # check that product vectors have same length
  if(length(product_vals) != length(product_pos)){
    stop("result vectors not same length")
  }

  # return new sparse_numeric object
  return(new("sparse_numeric", value = product_vals, pos = as.integer(product_pos), length = x@length))
})


# Subtraction Function
#' Subtract Two Sparse Numeric Vectors
#'
#' subtraction of two sparse vector
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param e1 sparse_numeric object
#' @param e2 sparse_numeric object
#' @param ... Additional arguments, not used
#' @return sparse_numeric object containing difference
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {

  # Check x y same length
  if (x@length != y@length){stop("vectors are not same length")}

  # Find positions where either x or y has value
  all_pos_sub = sort(unique(c(x@pos, y@pos)))

  # Initialize result vectors
  diff_pos = c()
  diff_vals = c()

  # Loop through all positions
  for (pos in all_pos_sub) {
    val = 0

    if (pos %in% x@pos){
      idx = which(x@pos == pos)
      val = val + x@value[idx]
    }

    if (pos %in% y@pos){
      idx = which(y@pos == pos)
      val = val - y@value[idx]
    }

    # Save if non-zero
    if (val != 0){
      diff_vals = c(diff_vals, val)
      diff_pos = c(diff_pos, pos)
    }
  }

  if(length(diff_vals) == 0){
    diff_vals = numeric(0)
    diff_pos = integer(0)
  }

  # Check if vectors same length
  if(length(diff_vals) != length(diff_pos)){
    stop("result vectors not same length")
  }

  # Return new sparse_numeric object
  return(new("sparse_numeric", value = diff_vals, pos = as.integer(diff_pos), length = x@length))

})


# Dot Product Function
#' Dot Product/Cross Product (?) of Sparse Numeric Vectors
#'
#' dot product of two sparse numeric vectors.
#'
#' @param x sparse_numeric object
#' @param y sparse_numeric object
#' @param ... Additional arguments, not used
#' @return value representing dot product
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  # Check x y same length
  if (x@length != y@length){
    stop("vectors are not same length")
  }

  # Find positions where both have values
  common_pos = intersect(x@pos, y@pos)

  result = 0
  # loop through
  for (pos in common_pos) {
    x_idx = which(x@pos == pos)
    x_val = x@value[x_idx]

    y_idx =  which(y@pos == pos)
    y_val = y@value[y_idx]

    result = result + (x_val * y_val)
  }

  return(result)
})


# Operator Overloads

#' @rdname sparse_add
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) {
            sparse_add(e1, e2)
          })

#' @rdname sparse_sub
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2){
            sparse_sub(e1, e2)
          })

#' @rdname sparse_mult
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2){
            sparse_mult(e1, e2)
          })

# coercion methods
#' Convert Numeric Vector to Sparse Numeric Vector
#'
#' @param from A numeric vector
#' @name coerce-numeric-sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz_pos = which(from != 0)
  nz_vals = from[nz_pos]
  len = as.integer(length(from))
  new("sparse_numeric", value = nz_vals, pos = as.integer(nz_pos), length = len)
})

#sparse_numeric -> numeric
#' Convert Sparse Numeric Vector to Numeric Vector
#'
#' @param from A sparse_numeric object
#' @name coerce-sparse_numeric-numeric
setAs("sparse_numeric", "numeric", function(from) {
  x = numeric(from@length)
  x[from@pos] = from@value
  return(x)
})

#Display Methods

# Convert plot to S4 generic?
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

# Show method for sparse_numeric
#' Show Method for Sparse Numeric Vectors
#'
#' @param object A sparse_numeric object
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse Numeric Vector (length =", object@length, ")\n")

  if (length(object@value) == 0) {
    cat("Non-zero values: none\n")
  } else {
    cat("Non-zero values:\n")
    print(data.frame(position = object@pos, value = object@value))
  }
})

# Plot method for sparse_numeric
#' Plot Method for Comparing Two Sparse Numeric Vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional plotting parameters
#' @export
setMethod("plot", c(x = "sparse_numeric", y = "sparse_numeric"), function(x, y, ...) {
  max_len = max(as.numeric(x@length), as.numeric(y@length))

  all_values = c(as.numeric(x@value), as.numeric(y@value))
  if (length(all_values) == 0) {
    all_values = c(0, 1)
  } else {
    all_values = c(all_values, 0)
  }

  graphics::plot.default(1, type = "n",
       xlim = c(1, max_len), ylim = range(all_values),
       xlab = "Position", ylab = "Value", main = "Sparse Vector Comparison")

  if (length(x@pos) > 0){
    points(as.numeric(x@pos), as.numeric(x@value), col = "blue", pch = 19)
  }

  if (length(y@pos) > 0){
    points(as.numeric(y@pos), as.numeric(y@value), col = "red", pch = 17)
  }

  legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = c(19, 17))
})

# Statistical Methods

# sum method
#' Sum of Sparse Numeric Vector
#'
#' @param x sparse_numeric object
#' @param ... Additional arguments passed to sum
#' @param na.rm logical, should NA values be removed
#' @export
setMethod("sum", "sparse_numeric", function(x, ...) {
  sum(x@value, ...)
})

#' Mean of Sparse Numeric Vector
#'
#' computes mean of sparse numeric vector
#'
#' @name mean
#'
#' @param x A sparse_numeric object
#' @param trim trim value
#' @param na.rm logical, should missing values be removed?
#' @param ... Additional arguments (not used)
#' @return mean of vector
#' @export
setGeneric("mean", function(x, ...) standardGeneric("mean"))

#' @rdname mean
#' @export
setMethod("mean", "sparse_numeric", function(x, trim = 0, na.rm = FALSE, ...) {
  if (trim > 0) {
    stop("trimmed means are not implemented for sparse_numeric")
  }

  total_sum <- sum(x@value, na.rm = na.rm)
  total_sum / as.numeric(x@length)
})


# norm() method
#'  Norm of Sparse Numeric Vector
#'
#' Computes norm of sparse vector
#'
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (not used)
#' @return The  norm of the vector
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  normsq = sum(x@value * x@value)
  return(sqrt(normsq))
})


#' Standardize Sparse Numeric Vector
#'
#' Standardize sparse numeric vector, by subtracting mean and dividing by standard deviation.
#'
#'
#'
#' @param x A sparse_numeric object
#' @param na.rm logical, should missing values be removed?
#' @param ... Additional arguments (not used)
#' @return  standardized sparse_numeric object
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, na.rm = FALSE, ...) {

  n = as.numeric(x@length)

  # sum and sum of squares over stored nonzeros
  s  = sum(x@value, na.rm = na.rm)
  ss = sum(x@value * x@value, na.rm = na.rm)

  # mean
  mu = s/n

  # ss - s^2/n = sum((xi - mu)^2)
  sqd = ss - (s * s)/n

  # If variance is zero → standardized vector is all zeros
  if (isTRUE(all.equal(sqd, 0))) {
    return(new("sparse_numeric",
               value = numeric(0),
               pos   = integer(0),
               length = x@length))
  }

  sd_sample = sqrt(sqd / (n - 1))

  dense_vec = as(x, "numeric")
  standardized_dense = (dense_vec - mu) / sd_sample

  # Convert back to sparse
  return(as(standardized_dense, "sparse_numeric"))
})


