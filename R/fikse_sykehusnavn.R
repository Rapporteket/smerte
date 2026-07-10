#' Add variable `orgname` if missing, update values if existing
#'
#' @param df data.frame, must contain variable `reshIdVar`
#' @return data.frame with variable `orgname`. Old values are overwritten
#' if `orgname` already existed if `df`
#'
#' @export
#' @examples
#' x <- data.frame(UnitId = as.character(c(108141, 109880, NA, 123, 105502)))
#' x %>% fikse_sykehusnavn(reshIdVar = "UnitId")
#'
fikse_sykehusnavn <- function(df, reshIdVar = UnitId) {

  sykehusoversikt = tibble::tribble(
    ~ "reshID", ~"orgname"             , ~"SykehusNavn"                    , ~"SykehusKortnavn",
    0         , "Nasjonal"             , "Nasjonal"                        , "Nasjonal",
    100089    , "Ahus"                 , "Ahus"                            , "AHUS",
    100082    , "HUS"                  , "Haukeland"                       , "HUS",
    4214288   , "Levanger"             , "Levanger"                        , "Levanger",
    4201115   , "Møre og Romsdal"      , "Møre og Romsdal"                 , "Møre og Romsdal",
    4207789   , "OUS Ullevål"          , "OUS, Ullevål"                    , "OUS, Ullevål",
    705758    , "OUS Radiumhospitalet" , "OUS, Radiumhospitalet"           , "OUS, Radiumhospitalet",
    705652    , "OUS Rikshospitalet"   , "OUS, Rikshospitalet"             , "OUS, Rikshospitalet",
    100320    , "St.Olavs hospital"    , "St.Olavs hospital"               , "St.Olavs hospital",
    101719    , "UNN"                  , "Universitetsykehuset Nord-Norge" , "UNN",
    4204083   , "Vestre Viken"         , "Vestre Viken"                    , "Vestre Viken"   ,
    100084    , "Fonna"                , "Fonna"                           , "Fonna"          ,
    100133    , "Sørlandet"            , "Sørlandet"                       , "Sørlandet",
    100083    , "Helse Stavanger"      , "Helse stavanger"                 , "SUS"
  )

  if (!(reshIdVar %in% names(df))) stop(paste0("df must contain variable: ", reshIdVar))

  sykehusoversikt_temp = sykehusoversikt %>%
    dplyr::mutate(reshID = as.character(reshID)) %>%
    dplyr::rename(!!reshIdVar := reshID)

  dplyr::left_join(df, sykehusoversikt_temp, by = reshIdVar)

}
