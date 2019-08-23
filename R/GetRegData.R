#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param reshId String providing reshId
#' @param startDate Start date ...
#' @param endDate End date...
#' @name getRegData
#' @aliases getRegDataTilsynsrapportMaaned getLocalYears
#' getRegDataLokalTilsynsrapportMaaned
NULL


#' @rdname getRegData
#' @export
getRegDataLokalTilsynsrapportMaaned <- function(registryName, reshId, startDate,
                                           endDate) {

  dbType <- "mysql"
  registryName <- paste0(registryName, reshId)

  query <- "
SELECT
  var.AntTilsLege,
  var.AntTilsSykPleier,
  var.AntTilsFysioT,
  var.AntTilsPsyk,
  var.AntTilsSosio,
  var.AntPasTils,
  var.Tilsett,
  var.RegDato11,
  var.InnlAvd,
  var.PasientID,
  var.ForlopsID,
  avd.DEPARTMENT_ID,
  avd.DEPARTMENT_NAME
FROM
  AlleVarNum var
LEFT JOIN
  avdelingsoversikt avd
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  var.AvdRESH = "

  query <- paste0(query, reshId, " AND (DATE(var.RegDato11) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  regData <- rapbase::LoadRegData(registryName, query, dbType)

  return(regData)
}


#' @rdname getRegData
#' @export
getLocalYears <- function(registryName, reshId) {

  dbType <- "mysql"
  registryName <- paste0(registryName, reshId)

  query <- "
SELECT
  YEAR(RegDato11) as year
FROM
  AlleVarNum
GROUP BY
  YEAR(RegDato11);
"
  rapbase::LoadRegData(registryName, query, dbType)
}
