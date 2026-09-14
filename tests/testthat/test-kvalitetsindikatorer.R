
context("ki_behandlertilsyn")

test_that("ki_behandlertilsyn gir forventet resultat", {

testdata <- data.frame(
    AntTilsLege      = c(0L, 1L, 2L, 5L, NA_integer_, NA_integer_),
    AntTilsSykPleier = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsFysioT    = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsPsyk      = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsSosio     = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsKonfLege  = c(0L, 0L, 0L, 0L, NA_integer_, 0L)
  )

  expect_equal(ki_behandlertilsyn(testdata) |>
                 select(ki_teller),
               c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(ki_behandlertilsyn(testdata) |> select(ki_nevner),
               rep(TRUE, 6))
  expect_error(ki_behandlertilsyn(testdata |> select(-AntTilsPsyk)),
               "Variabelen 'AntTilsPsyk' må være med i inndata. ")

})
