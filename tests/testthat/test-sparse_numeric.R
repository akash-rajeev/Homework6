# Tests for sparse_numeric class and methods


# VALIDITY TESTS

test_that("check validity method exists", {
  expect_false({
    validity_method = getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x = new("sparse_numeric",
            value = c(1, 2, 3, 1),
            pos = c(1L, 2L, 3L, 5L),
            length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x = new("sparse_numeric",
            value = c(1, 2, 3, 1),
            pos = c(1L, 2L, 3L, 5L),
            length = 5L)
    x@length = 2L
    validObject(x)
  })
})

test_that("unsorted positions", {
  expect_error({
    x = new("sparse_numeric",
            value = c(1, 2, 3),
            pos = c(3L, 1L, 2L),
            length = 5L)
    validObject(x)
  })
})

test_that("duplicates", {
  expect_error({
    x = new("sparse_numeric",
            value = c(1, 2, 3),
            pos = c(1L, 2L, 2L),
            length = 5L)
    validObject(x)
  })
})

test_that("zero values", {
  expect_error({
    x = new("sparse_numeric",
            value = c(1, 0, 3),
            pos = c(1L, 2L, 3L),
            length = 5L)
    validObject(x)
  })
})

# COERCION TESTS

test_that("check coercion return class", {
  expect_s4_class({
    x = as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("coercion from numeric works correctly", {
  x = as(c(0, 5, 0, -3, 0), "sparse_numeric")
  expect_equal(x@value, c(5, -3))
  expect_equal(x@pos, c(2L, 4L))
  expect_equal(x@length, 5L)
})

test_that("coercion to numeric works correctly", {
  x = new("sparse_numeric",
          value = c(5, -3),
          pos = c(2L, 4L),
          length = 5L)
  result = as(x, "numeric")
  expect_equal(result, c(0, 5, 0, -3, 0))
})

test_that("coercion handles all zeros", {
  x = as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(length(x@value), 0)
  expect_equal(length(x@pos), 0)
  expect_equal(x@length, 4L)
})

# METHOD EXISTENCE TESTS


test_that("show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("+ method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("- method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("* method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})


# ADDITION TESTS

test_that("check returned class for add", {
  expect_s4_class({
    x = as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y = as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result = as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x = as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y = as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result = as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x = as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y = as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x = as(rep(0, 10), "sparse_numeric")
    y = as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

test_that("addition with + operator works", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 0, 0, -3), "sparse_numeric")
  result = x + y
  expect_s4_class(result, "sparse_numeric")
  expect_equal(as(result, "numeric"), c(1, 5, 0, -6))
})

test_that("addition cancels to zero", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(0, -5, 0, 3), "sparse_numeric")
  result = sparse_add(x, y)
  expect_equal(length(result@value), 0)
})

# SUBTRACTION TESTS


test_that("sparse_sub test", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 0, 0, -3), "sparse_numeric")
  result = sparse_sub(x, y)
  expect_equal(as(result, "numeric"), c(-1, 5, 0, 0))
})

test_that("subtraction with - operator works", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 0, 0, -3), "sparse_numeric")
  result = x - y
  expect_s4_class(result, "sparse_numeric")
})

test_that("subtraction wrong length error", {
  expect_error({
    x = as(c(0, 5, 0, -3), "sparse_numeric")
    y = as(c(1, 0, 0), "sparse_numeric")
    sparse_sub(x, y)
  })
})

# MULTIPLICATION TESTS

test_that("sparse_mult basic test", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 2, 0, -3), "sparse_numeric")
  result = sparse_mult(x, y)
  expect_equal(as(result, "numeric"), c(0, 10, 0, 9))
})

test_that("multiplication with * operator works", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 2, 0, -3), "sparse_numeric")
  result = x * y
  expect_s4_class(result, "sparse_numeric")
})

test_that("multiplication no overlap", {
  x = as(c(1, 0, 0, 0), "sparse_numeric")
  y = as(c(0, 2, 0, 0), "sparse_numeric")
  result = sparse_mult(x, y)
  expect_equal(length(result@value), 0)
})

test_that("multiplication wrong length error", {
  expect_error({
    x = as(c(0, 5, 0, -3), "sparse_numeric")
    y = as(c(1, 2), "sparse_numeric")
    sparse_mult(x, y)
  })
})

# CROSS PRODUCT TESTS

test_that("sparse_crossprod basic test", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 2, 0, -3), "sparse_numeric")
  result = sparse_crossprod(x, y)
  expect_equal(result, 19)
})

test_that("crossprod with no overlap", {
  x = as(c(1, 0, 0, 0), "sparse_numeric")
  y = as(c(0, 2, 0, 0), "sparse_numeric")
  result = sparse_crossprod(x, y)
  expect_equal(result, 0)
})

test_that("crossprod wrong length error", {
  expect_error({
    x = as(c(0, 5, 0, -3), "sparse_numeric")
    y = as(c(1, 2), "sparse_numeric")
    sparse_crossprod(x, y)
  })
})

# SUM AND MEAN TESTS

test_that("sum method works", {
  x = as(c(0, 5, 0, -3, 0, 0, 10), "sparse_numeric")
  expect_equal(sum(x), 12)
})

test_that("sum of all zeros", {
  x = as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(sum(x), 0)
})

test_that("mean method works", {
  x = as(c(0, 5, 0, -3, 0, 0, 10), "sparse_numeric")
  x_dense = c(0, 5, 0, -3, 0, 0, 10)
  expect_equal(mean(x), mean(x_dense))
})

test_that("mean of all zeros", {
  x = as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(mean(x), 0)
})

# NORM TESTS

test_that("norm basic test", {
  x = as(c(0, 3, 0, 4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("norm test 2", {
  x = as(c(0, 5, 0, -3, 0, 0, 10), "sparse_numeric")
  x_dense = c(0, 5, 0, -3, 0, 0, 10)
  expect_equal(norm(x), sqrt(sum(x_dense^2)))
})

test_that("norm zero vector", {
  x = as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(norm(x), 0)
})

# STANDARDIZE TESTS

test_that("standardize basic test", {
  x = as(c(0, 5, 0, -3, 0, 0, 10), "sparse_numeric")
  x_std = standardize(x)
  x_dense = c(0, 5, 0, -3, 0, 0, 10)
  x_dense_std = scale(x_dense)[,1]
  expect_equal(as(x_std, "numeric"), x_dense_std)
})

test_that("standardize constant vector", {
  x = as(c(5, 5, 5, 5, 5), "sparse_numeric")
  x_std = standardize(x)
  expect_equal(length(x_std@value), 0)
})

test_that("standardize mostly zeros", {
  x = as(c(0, 0, 3, 0, 0), "sparse_numeric")
  x_std = standardize(x)
  x_dense = c(0, 0, 3, 0, 0)
  x_dense_std = scale(x_dense)[,1]
  expect_equal(as(x_std, "numeric"), x_dense_std)
})

test_that("standardize creates new nonzeros", {
  x = as(c(0, 5, 0, -3, 0, 0, 10), "sparse_numeric")
  x_std = standardize(x)
  # after standardization, the zeros should have values
  expect_true(length(x_std@value) > length(x@value))
})

# SHOW METHOD TESTS

test_that("show method test", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  expect_output(show(x), "Sparse Numeric Vector")
  expect_output(show(x), "length = 4")
})

test_that("show method for empty vector", {
  x = as(c(0, 0, 0), "sparse_numeric")
  expect_output(show(x), "none")
})


# PLOT METHOD TESTS

test_that("plot method test", {
  x = as(c(0, 5, 0, -3), "sparse_numeric")
  y = as(c(1, 0, 0, -3), "sparse_numeric")
  expect_no_error(plot(x, y))
})
