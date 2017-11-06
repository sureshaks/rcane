library(rcane)
context("Correctness")
test_that("helloworld",{expect_equal(as.vector(
  round(batch(c(1,2,3),c(1,2,3), alpha=0.001))), c(0,1))})

