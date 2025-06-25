#' Add variable `orgname` if missing, update values if existing
#'
#' @param df data.frame, must contain variable `UnitId`
#' @return data.frame with variable `orgname`. Old values are overwritten
#' if `orgname` already existed if `df`
#'
#' @export
#' @examples
#' x <- data.frame(UnitId = c(108141, 109880, NA, 123, 105502))
#' x %>% fikse_sykehusnavn()
#'
#' y <- data.frame(UnitId = c(108141, 109880, NA, 123, 105502),
#'                 orgname = c("AHUS", "Ullevål", "NA",
#'                  "test", "Stavanger"))
fikse_sykehusnavn <- function(df) {

  if (!("UnitId" %in% names(df))) stop("df must contain variable UnitId")

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
      TRUE ~ NA_character_
    ))
}
