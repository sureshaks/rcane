library(testthat)
library(rcane)
library(tidyverse)

context("input parameter check")

test_that("check for valid methods", {
  expect_error(rcane::rlm(cty~hwy, mpg, method='bgd'), NA)
  expect_error(rcane::rlm(cty~hwy, mpg, method='sgd'), NA)
  expect_error(rcane::rlm(cty~hwy, mpg, method='mini-bgd'), NA)
  expect_error(rcane::rlm(cty~hwy, mpg, method='cd'), NA)
  expect_error(rcane::rlm(cty~hwy, mpg, method='xxx'))
  expect_error(rcane::rlm(cty~hwy, mpg, method=''))
  expect_error(rcane::rlm(cty~hwy, mpg, method='bgdsgd'))
})

context("input matrix check")

test_that("Check parameters in dataframe", {
  expect_error(rcane::rlm(cty~hwy, mpg, method='bgd'), NA)
  expect_error(rcane::rlm(cty~trans, mpg, method='bgd'), NA)
  expect_error(rcane::rlm(cty~trans + hwy, mpg, method='bgd'), NA)
  expect_error(rcane::rlm(cty~., mpg, method='bgd'), NA)
  expect_error(rcane::rlm(xxx~hwy, mpg, method='bgd'))
  expect_error(rcane::rlm(cty~xxx, mpg, method='bgd'))
  expect_error(rcane::rlm(cty~5566, mpg, method='bgd'))
  expect_error(rcane::rlm(xxx~xxx, mpg, method='bgd'))
  expect_warning(rcane::rlm(model~cty, mpg, method='bgd'))
})

f <- cty~hwy
f[[1]] = `+`

test_that("Invalid formula", {
  expect_error(rcane::rlm(f, mpg, method='bgd'))
})

df.test <- data.frame(x = numeric(), y=numeric())

test_that("empty row", {
  expect_error(rcane::rlm(x~y, df.test, method='bgd'))
})

df.test.2 <- data.frame(x = c(NA,NA,NA), y = c(NA,NA,NA))
df.test.3 <- data.frame(x = c(NA, 3, NA),y = c(3, 3, NA))

test_that("NA in data frame", {
  expect_error(rcane::rlm(x~y, df.test.2, method='bgd'))
  expect_error(rcane::rlm(x~y, df.test.3, method='bgd'), NA)
})

context("Regression")
set.seed(123)

TRUE_B0 = 2
TRUE_B1 = 3

x1 <- runif(10000, -2, 2)
e <- rnorm(10000, 0, 4)
y <- x1*TRUE_B0 + TRUE_B1 + e

df.test4 = data.frame(x1, x2, y)

test_that("manual created dataframe", {
  expect_equal(coef(rcane::rlm(y~x1+x2, df.test4, method="bgd")), 
                coef(lm(y~x1+x2, df.test4)),
                tolerance = 0.001)
  expect_equal(coef(rcane::rlm(y~x1+x2, df.test4, method="mini-bgd")), 
               coef(lm(y~x1+x2, df.test4)),
               tolerance = 0.001)
  #expect_equal(coef(rcane::rlm(y~x1+x2, df.test4, method="cd")), 
  #             coef(lm(y~x1+x2, df.test4)),
  #             tolerance = 0.001)
  expect_equal(coef(rcane::rlm(y~x1+x2, df.test4, method="sgd")), 
                    coef(lm(y~x1+x2, df.test4)),
                    tolerance = 0.001)
})

test_that("existing dataframe",{
  expect_equal(coef(rcane::rlm(cty~hwy, mpg, method="bgd", alpha=0.001, max.iter=50000, precision=0.000000001)),
               coef(lm(cty~hwy, mpg)),
               tolerance = 0.001
  )
  expect_equal(coef(rcane::rlm(cty~hwy, mpg, method="sgd", alpha=1, max.iter=1000, precision=0.0000001, AdaGrad=TRUE)),
               coef(lm(cty~hwy, mpg)),
               tolerance = 0.1
  )
})
