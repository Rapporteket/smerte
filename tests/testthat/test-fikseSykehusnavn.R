test_that("Funksjonen returnerer forventet utdata", {

  d_test = tibble::tibble(
    UnitId = c("100089", "4201115", "705758")
  )

  d_forventet = tibble::tibble(
    UnitId = c("100089", "4201115", "705758"),
    orgname = c("Ahus", "Møre og Romsdal", "OUS Radiumhospitalet"),
    SykehusNavn = c("Ahus", "Møre og Romsdal", "OUS, Radiumhospitalet"),
    SykehusKortnavn = c("AHUS", "Møre og Romsdal", "OUS, Radiumhospitalet")
  )

  expect_identical(fikse_sykehusnavn(d_test, reshIdVar = "UnitId"),
               d_forventet)
})

test_that("Funksjonen fungerer med annet variabelnavn for reshId", {

  d_test = tibble::tibble(
    sykehusID = c("100089", "4201115", "705758")
  )

  d_forventet = tibble::tibble(
    sykehusID = c("100089", "4201115", "705758"),
    orgname = c("Ahus", "Møre og Romsdal", "OUS Radiumhospitalet"),
    SykehusNavn = c("Ahus", "Møre og Romsdal", "OUS, Radiumhospitalet"),
    SykehusKortnavn = c("AHUS", "Møre og Romsdal", "OUS, Radiumhospitalet")
  )

  expect_identical(fikse_sykehusnavn(d_test, reshIdVar = "sykehusID"),
                   d_forventet)

})

test_that("Funksjonen gir forventet advarsel og utdata hvis det finnes ukjent reshID", {

  d_test = tibble(
    UnitId = c("100089", "4201115", "123456789")
  )
  d_test_ekstra = tibble(
    UnitId = c("100089", "4201115", "123456789", "987654321")
    )

  d_forventet = tibble(
    UnitId = c("100089", "4201115", "123456789"),
    orgname = c("Ahus", "Møre og Romsdal", NA_character_),
    SykehusNavn = c("Ahus", "Møre og Romsdal", NA_character_),
    SykehusKortnavn = c("AHUS", "Møre og Romsdal", NA_character_)
  )

  d_forventet2 = d_forventet |>
    bind_rows(
      tibble(
        UnitId = "987654321",
        orgname = NA_character_,
        SykehusNavn = NA_character_,
        SykehusKortnavn = NA_character_)
      )

  d_res = expect_warning(
    fikse_sykehusnavn(d_test, reshIdVar = "UnitId"),
    regexp = "^ReshID: '123456789' finnes ikke i tabell\\.$"
  )

  d_res2 = expect_warning(
    fikse_sykehusnavn(d_test_ekstra, reshIdVar = "UnitId"),
    regexp = "^ReshID: '123456789', '987654321' finnes ikke i tabell\\.$"
  )

  # Litt kronglete test her, men det er for å sjekke at output er som forventet
  # tross advarsel som sjekkes ovenfor
  expect_identical(d_res, d_forventet)
  expect_identical(d_res2, d_forventet2)

})

#
test_that("Funksjonen gir forventet feilmelding hvis reshID-variabel mangler", {

  d_test = tibble::tibble(
    OtherId = c("100089", "4201115")
  )

  expect_error(
    fikse_sykehusnavn(d_test, reshIdVar = "UnitId"),
    "Inndata må inneholde variabelen: 'UnitId'"
  )
})


