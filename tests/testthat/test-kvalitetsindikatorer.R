
context("ki_behandlertilsyn")

test_that("ki_behandlertilsyn gir forventet resultat", {

testdata <- tibble(
    AntTilsLege      = c(0L, 1L, 2L, 5L, NA_integer_, NA_integer_),
    AntTilsSykPleier = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsFysioT    = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsPsyk      = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsSosio     = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsKonfLege  = c(0L, 0L, 0L, 0L, NA_integer_, 0L)
  )

testresultat = tibble(
  AntTilsLege      = c(0L, 1L, 2L, 5L, 0L, 0L),
  AntTilsSykPleier = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsFysioT    = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsPsyk      = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsSosio     = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsKonfLege  = c(0L, 0L, 0L, 0L, 0L, 0L),
  ki_krit_nevner   = c(rep(TRUE,6)),
  ki_krit_teller   = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(ki_behandlertilsyn(testdata),
               testresultat)

  expect_error(ki_behandlertilsyn(testdata |> select(-AntTilsPsyk)),
               "'AntTilsPsyk' må være med i inndata.")
  expect_error(ki_behandlertilsyn(testdata |> select(-AntTilsPsyk, -AntTilsSosio, -AntTilsKonfLege)),
               "'AntTilsPsyk', 'AntTilsSosio' og 'AntTilsKonfLege' må være med i inndata.")


})
