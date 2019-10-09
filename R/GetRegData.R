#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param reshId String providing reshId
#' @param year Integer four digit year to be reported from
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataLokalTilsynsrapportMaaned getLocalYears
NULL


#' @rdname getRegData
#' @export
getRegDataLokalTilsynsrapportMaaned <- function(registryName, reshId, year, ...) {

  dbType <- "mysql"
  registryName <- paste0(registryName, reshId)

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Load data from", registryName))
  }

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

  query <- paste0(query, "'", reshId, "'", " AND YEAR(var.RegDato11)=",
                  year, ";")

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
