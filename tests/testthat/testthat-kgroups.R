library(testthat)
library(onewaytests)

test_that("af.test works", {
  result <- af.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("ag.test works", {
  result <- ag.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("agp.test works", {
  result <- agp.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("aov.test works", {
  result <- aov.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("ap.test works", {
  result <- ap.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("aw.test works", {
  result <- aw.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("b2.test works", {
  result <- b2.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("bf.test works", {
  result <- bf.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("box.test works", {
  result <- box.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("cochran.test works", {
  result <- cochran.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("gp.test (generalized test equivalent to parametric bootstrap test) works", {
  result <- gp.test(Sepal.Length ~ Species, data = iris, method = "gtb")
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("gp.test (generalized test equivalent to fiducial test) works", {
  result <- gp.test(Sepal.Length ~ Species, data = iris, method = "gtf")
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("james.test works", {
  result <- james.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "jt")
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("johansen.test works", {
  result <- johansen.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("kw.test works", {
  result <- kw.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("mbf.test works", {
  result <- mbf.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("pf.test works", {
  result <- pf.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("ss.test works", {
  result <- ss.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("wa.test works", {
  result <- wa.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("welch.test works", {
  result <- welch.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("wgf.test works", {
  result <- wgf.test(Sepal.Length ~ Species, data = iris) 
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})
