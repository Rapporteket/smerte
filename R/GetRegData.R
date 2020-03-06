#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param year Integer four digit year to be reported from
#' @param reshId String providing organization Id
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataLokalTilsynsrapportMaaned getLocalYears
NULL


#' @rdname getRegData
#' @export
getRegDataLokalTilsynsrapportMaaned <- function(registryName, reshId, userRole,
                                                year, ...) {

  dbType <- "mysql"

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Load data from", registryName))
  }

  # special case at OUS
  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$ousAccess$reshId &&
      userRole %in% conf$reg$smerte$ousAccess$userRole) {
    deps <- paste0(conf$reg$smerte$ousAccess$reshId, collapse = ", ")
  } else {
    deps <- reshId
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
  var.StartdatoTO,
  var.InnlAvd,
  var.PasientID,
  var.ForlopsID,
  avd.DEPARTMENT_ID,
  avd.DEPARTMENT_NAME,
  avd.DEPARTMENT_SHORTNAME
FROM
  AlleVarNum var
LEFT JOIN
  avdelingsoversikt avd
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  YEAR(var.RegDato11) = "

  query <- paste0(query, year, " AND var.AvdRESH IN (", deps, ");")

  print(query)

  rapbase::LoadRegData(registryName, query, dbType)
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
