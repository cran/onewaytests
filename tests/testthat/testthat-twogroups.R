library(testthat)
library(onewaytests)
library(AID)

data(AADT)

test_that("mw.test works", {
  result <- mw.test(aadt ~ control, data = AADT)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("st.test works", {
  result <- st.test(aadt ~ control, data = AADT)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("wt.test works", {
  result <- st.test(aadt ~ control, data = AADT)
  expect_s3_class(result, "owt")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})
