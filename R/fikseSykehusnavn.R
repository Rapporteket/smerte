#' Legg til sykehusnavn
#'
#' @description Legger til variablene `orgname`, `SykehusNavn` og
#' `SykehusKortnavn` hvis disse ikke finnes i datasettet `d`.
#'
#' @param d tibble eller data.frame, som inneholder variabelen `reshIdVar`.
#' @param reshIdVar Tekststreng for å identifisere hvilken variabel som inneholder
#' ReshId for enhetene. Kan varierer mellom ulike tabeller.
#'
#' @return Returnerer opprinnelig datasett i tillegg til kolonnene `orgname`,
#' `SykehusNavn` og `SykehusKortnavn` hvis disse ikke finnes fra før.
#'
#' @export
#' @examples
#' x <- data.frame(UnitId = as.character(c(100089, 4201115, NA, 705758, 4204083)))
#' x |> fikse_sykehusnavn(reshIdVar = "UnitId")
fikse_sykehusnavn <- function(d, reshIdVar = "UnitId") {

  sykehusoversikt = tribble(
    ~ "reshID", ~"orgname"             , ~"SykehusNavn"                    , ~"SykehusKortnavn",
    "0"         , "Nasjonal"             , "Nasjonal"                        , "Nasjonal",
    "100089"    , "Ahus"                 , "Ahus"                            , "AHUS",
    "100082"    , "HUS"                  , "Haukeland"                       , "HUS",
    "4214288"   , "Levanger"             , "Levanger"                        , "Levanger",
    "4201115"   , "Møre og Romsdal"      , "Møre og Romsdal"                 , "Møre og Romsdal",
    "4207789"   , "OUS Ullevål"          , "OUS, Ullevål"                    , "OUS, Ullevål",
    "705758"    , "OUS Radiumhospitalet" , "OUS, Radiumhospitalet"           , "OUS, Radiumhospitalet",
    "705652"    , "OUS Rikshospitalet"   , "OUS, Rikshospitalet"             , "OUS, Rikshospitalet",
    "100320"    , "St.Olavs hospital"    , "St.Olavs hospital"               , "St.Olavs hospital",
    "101719"    , "UNN"                  , "Universitetsykehuset Nord-Norge" , "UNN",
    "4204083"   , "Vestre Viken"         , "Vestre Viken"                    , "Vestre Viken"   ,
    "100084"    , "Fonna"                , "Fonna"                           , "Fonna"          ,
    "100133"    , "Sørlandet"            , "Sørlandet"                       , "Sørlandet",
    "100083"    , "Helse Stavanger"      , "Helse stavanger"                 , "SUS"
  )

  if (!(reshIdVar %in% names(d))) stop(paste0("Inndata må inneholde variabelen: '", reshIdVar, "'"))

  ukjent_resh = setdiff(d[[reshIdVar]], sykehusoversikt$reshID)

  if (length(ukjent_resh) > 0L) {
    warning(
      "ReshID: ",
      paste0("'", ukjent_resh, "'", collapse = ", "),
      " finnes ikke i tabell.",
      call. = FALSE
    )
  }


  sykehusoversikt_temp = sykehusoversikt |>
    rename(!!reshIdVar := reshID)

  left_join(d, sykehusoversikt_temp, by = reshIdVar)

}
