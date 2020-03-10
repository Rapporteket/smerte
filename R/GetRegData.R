#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param year Integer four digit year to be reported from
#' @param reshId String providing organization Id
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataLokalTilsynsrapportMaaned getLocalYears getHospitalName
NULL


.getDeps <- function(reshId, userRole) {

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$ousAccess$reshId &&
      userRole %in% conf$reg$smerte$ousAccess$userRole) {
    return(paste0(conf$reg$smerte$ousAccess$reshId, collapse = ", "))
  } else {
    return(reshId)
  }
}

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
  deps <- .getDeps(reshId, userRole)

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

  rapbase::LoadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getLocalYears <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  YEAR(RegDato11) as year
FROM
  AlleVarNum
WHERE
  AvdRESH IN (", deps, ")
GROUP BY
  YEAR(RegDato11);
")

  rapbase::LoadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getHospitalName <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  LOCATIONNAME AS ln
FROM
  avdelingsoversikt
WHERE
  DEPARTMENT_CENTREID IN (", deps, ")
GROUP BY
  LOCATIONNAME;
                  ")

  df <- rapbase::LoadRegData(registryName, dbType = dbType, query = query)
  n <- dim(df)[1]
  hVec <- df[1:n, 1]
  if (n > 1) {
    hStr <- paste(hVec[1:n-1], sep = ", ")
    hStr <- paste(hStr, hVec[n], sep = " og ")
  } else {
    hStr <- paste(hVec)
  }

  hStr

}
