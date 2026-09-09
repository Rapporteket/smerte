test_that("Returnerer forventet utdata", {

  # Filtrerer bort uønsket LANGUAGEID,
  # Filtrerer bort skjemanavn som ikke er i tabeller-vektor.
  # Filtrerer bort rader som ikke har label og nivå.
  # Returnerer forventet utdata for korrekte rader.

  testdata <- tibble::tribble(
    ~ID,                         ~LANGUAGEID, ~TEXT,
    "HADS_KATEGORI_L_1_D",        "no",        "Kategori 1",
    "HADS_KATEGORI_L_2_D",        "no",        "Kategori 2",
    "HADS_KATEGORI_L_3_D",        "no",        "Kategori 3",
    "HADS_KATEGORI_L_1_D",        "en",        "Kategori 4",
    "UKJENT_TABELL_L_1_D",        "no",        "Kategori 1",
    "HADS_KATEGORI_D",            "no",        "Dette er ikke en kategori"
  )

  kb_test <- lag_smerte_kb_kategoriske(testdata)

  d_forventet <- tibble::tribble(
    ~tabell, ~variabel_id, ~verdi, ~verditekst,
    "HADS",   "KATEGORI",     1L, "Kategori 1",
    "HADS",   "KATEGORI",     2L, "Kategori 2",
    "HADS",   "KATEGORI",     3L, "Kategori 3"
  )

  expect_equal(kb_test, d_forventet)
})

test_that("Håndterer variabler med '_'", {
  d_test <- tibble::tibble(
    ID = "PATIENT_FLERE_ORD_I_VARIABELNAVN_L_12_D",
    LANGUAGEID = "no",
    TEXT = "Dette er label"
  )

  d_forventet = tibble::tibble(
    tabell = "PATIENT",
    variabel_id = "FLERE_ORD_I_VARIABELNAVN",
    verdi = 12L,
    verditekst = "Dette er label"
  )

  expect_equal(lag_smerte_kb_kategoriske(d_test),d_forventet)
})

test_that("Håndterer høye/lave verdier som forventet", {

  d_test <- tibble::tribble(
    ~ID,                    ~LANGUAGEID, ~TEXT,
    "HADS_SCORE_L_-1_D",     "no",        "negativ verdi",
    "HADS_SCORE_L_9999_D",    "no",        "veldig høy"
  )

  d_resultat <- lag_smerte_kb_kategoriske(d_test)

  expect_equal(d_resultat$verdi, c(-1L, 9999L))
})


