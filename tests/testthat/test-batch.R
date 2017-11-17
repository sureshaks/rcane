library(rcane)
context("Correctness")
test_that("basic",
          {expect_equal(
            as.vector(
              round(
                rlm(y ~ x,
                    data=data.frame(x=c(1,2,3), y=c(1,2,3)))
                )), c(0,1))})

