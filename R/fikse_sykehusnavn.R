#' Add variable `orgname` if missing, update values if existing
#'
#' @param df data.frame, must contain variable `reshIdVar`
#' @return data.frame with variable `orgname`. Old values are overwritten
#' if `orgname` already existed if `df`
#'
#' @export
#' @examples
#' x <- data.frame(UnitId = c(108141, 109880, NA, 123, 105502))
#' x %>% fikse_sykehusnavn(reshIdVar = "UnitId")
#'
#' y <- data.frame(UnitId = c(108141, 109880, NA, 123, 105502),
#'                 orgname = c("AHUS", "Ullevål", "NA",
#'                  "test", "Stavanger"))
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

  df %>%
    dplyr::mutate(orgname = dplyr::case_when(
      UnitId == 0 ~ "Nasjonal",
      UnitId == 100089 ~ "Ahus",
      UnitId == 100082 ~ "HUS",
      UnitId == 4214288 ~ "Levanger",
      UnitId == 4201115 ~ "Møre og Romsdal",
      UnitId == 4207789 ~ "OUS Ullevål",
      UnitId == 705758 ~ "OUS Radiumhospitalet",
      UnitId == 705652 ~ "OUS Rikshospitalet",
      UnitId == 100320 ~ "St.Olavs hospital",
      UnitId == 101719 ~ "UNN",
      UnitId == 4204083 ~ "Vestre Viken",
      UnitId == 100084 ~ "Fonna",
      UnitId == 100133 ~ "Sørlandet",
      UnitId == 100083 ~ "Helse Stavanger",
      TRUE ~ NA_character_
    ))

  if (!(reshIdVar %in% names(df))) stop(paste0("df must contain variable: ", reshIdVar))

  sykehusoversikt_temp = sykehusoversikt %>%
    mutate(reshID = as.character(reshID)) %>%
    rename(!!reshIdVar := reshID)

  left_join(df, sykehusoversikt_temp, by = reshIdVar)

}
