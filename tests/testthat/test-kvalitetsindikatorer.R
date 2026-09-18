
context("ki_behandlertilsyn")

test_that("ki_behandlertilsyn gir forventet resultat", {

testdata <- tibble(
    AntTilsLege      = c(0L, 1L, 2L, 5L, NA_integer_, NA_integer_),
    AntTilsSykPleier = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsFysioT    = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsPsyk      = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsSosio     = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    AntTilsKonfLege  = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
    Tilsett          = c(1L, 1L, 1L, 1L, 4L, 1L)
  )

testresultat = tibble(
  AntTilsLege      = c(0L, 1L, 2L, 5L, 0L, 0L),
  AntTilsSykPleier = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsFysioT    = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsPsyk      = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsSosio     = c(0L, 0L, 0L, 0L, 0L, 0L),
  AntTilsKonfLege  = c(0L, 0L, 0L, 0L, 0L, 0L),
  Tilsett          = c(1L, 1L, 1L, 1L, 4L, 1L),
  ki_krit_nevner   = c(rep(TRUE,4), FALSE, TRUE),
  ki_krit_teller   = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(ki_behandlertilsyn(testdata),
               testresultat)

  expect_error(ki_behandlertilsyn(testdata |> select(-AntTilsPsyk)),
               "'AntTilsPsyk' må være med i inndata.")
  expect_error(ki_behandlertilsyn(testdata |> select(-Tilsett)),
               "'Tilsett' må være med i inndata.")
  expect_error(ki_behandlertilsyn(testdata |> select(-AntTilsPsyk, -AntTilsSosio, -AntTilsKonfLege)),
               "'AntTilsPsyk', 'AntTilsSosio' og 'AntTilsKonfLege' må være med i inndata.")
})

context("smerteendring")

# Pasient håndteres riktig hvis det finnes NA-verdier for aktuelle variabler
# ki_krit_teller er riktig beregnet avhengig av smerteendring, (>0, <0, =0)
# Forventet feilmelding gitt manglende variabler.
# Typekontroll for aktuelle variabler med forventet feilmelding.
library(testthat)

test_that("Funksjonen returnerer forventet resultat", {
  d_test <- tibble(
    id     = c(1, 2, 3, 4, 5),
    StSm12 = c(8, 5, 2, NA_integer_, 5),
    StSm21 = c(4, 5, 3, 3, NA_integer_),
    SvSm12 = c(3, 4, 1, NA_integer_, 5),
    SvSm21 = c(6, 2, 1, 4, NA_integer_)
  )

  d_forventet_sterk = tibble(
    id     = c(1, 2, 3, 4, 5),
    StSm12 = c(8, 5, 2, NA_integer_, 5),
    StSm21 = c(4, 5, 3, 3, NA_integer_),
    SvSm12 = c(3, 4, 1, NA_integer_, 5),
    SvSm21 = c(6, 2, 1, 4, NA_integer_),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  d_forventet_svak = tibble(
    id     = c(1, 2, 3, 4, 5),
    StSm12 = c(8, 5, 2, NA_integer_, 5),
    StSm21 = c(4, 5, 3, 3, NA_integer_),
    SvSm12 = c(3, 4, 1, NA_integer_, 5),
    SvSm21 = c(6, 2, 1, 4, NA_integer_),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    ki_krit_teller = c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_identical(ki_smerteendring(d_test, var = "sterkeste"),
                   d_forventet_sterk)
  expect_identical(ki_smerteendring(d_test, var = "svakeste"),
                   d_forventet_svak)
  # Feilmeldinger
  expect_error(ki_smerteendring(d_test, var = "medium"),
               "var må være enten 'sterkeste' eller 'svakeste'.")
  expect_error(ki_smerteendring(d_test |> select(-StSm12), "sterkeste"),
               "Variablene 'StSm12' og 'StSm21' må være i inndata for å beregne endring i sterkeste smerte. ")
  expect_error(ki_smerteendring(d_test |> select(-SvSm12), "svakeste"),
               "Variablene 'SvSm12' og 'SvSm21' må være i inndata for å beregne endring i svakeste smerte. ")

})
