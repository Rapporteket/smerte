
test_that("module input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(variabeloversiktInput("id")))
})
