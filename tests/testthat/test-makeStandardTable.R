test_that("function returns kable objects", {
  expect_true("kableExtra" %in% class(mst(tab = mtcars[1:10, ],
                                          type = "html")))
  expect_true("knitr_kable" %in% class(mst(tab = mtcars[1:10, ], lsd = FALSE)))
  expect_true("knitr_kable" %in% class(mst(tab = mtcars[1:10, ], lsd = TRUE)))
})
