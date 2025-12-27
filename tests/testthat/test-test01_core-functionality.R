test_that("core functionality works", {
  X <- scale(iris[,1:4])
  testthat::expect_no_failure({
    ng <- neuralgas( X, refresh = 1500 )
    print( ng )

  }
  )
})
