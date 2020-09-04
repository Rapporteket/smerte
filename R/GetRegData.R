#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param year Integer four digit year to be reported from
#' @param startDate String defing start of date range as YYYY-MM-DD
#' @param endDate String defing end of date range as YYYY-MM-DD
#' @param reshId String providing organization Id
#' @param userRole String providing user role
#' @param tableName String providing a table name
#' @param fromDate String providing start date
#' @param toDate String provideing end date
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataLokalTilsynsrapportMaaned
#' getRegDataRapportDekningsgrad getLocalYears getAllYears getHospitalName
#' getDataDump
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

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Load tilsynsrapport data from",
                                  registryName, ": ", query))
  }

  rapbase::LoadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getRegDataRapportDekningsgrad <- function(registryName, reshId, userRole,
                                          startDate, endDate, ...) {
  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- "
SELECT
  PasientID,
  ForlopsID,
  InklKritOppf,
  SkrSamtykke
FROM
  AlleVarNum
WHERE
  AvdRESH IN ("

  query <- paste0(query, deps, ") AND (DATE(StartdatoTO) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste0("Load data from ", registryName, ":", query))
  }

  rapbase::LoadRegData(registryName, query, dbType)
}

#' @rdname getRegData
#' @export
getRegDataIndikator <- function(registryName, reshId, userRole,
                                                year, ...) {

  dbType <- "mysql"

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
  var.HenvistDato,
  var.EvalSpm3,
  var.PRSpm3,
  var.SvSmRo12,
  var.SvSmRo21,
  var.StSmRo12,
  var.StSmRo21,
  var.SvSmBev12,
  var.SvSmBev21,
  var.SykehusNavn,
  var.StSmBev12,
  var.StSmBev21,
  var.PasientID,
  var.ForlopsID,
  var.InnlAvd
FROM
  AlleVarNum var
WHERE
  YEAR(var.RegDato11) = "

  if (isNationalReg(reshId)) {
    query <- paste0(query, year, ";")
  } else {
    query <- paste0(query, year, " AND var.AvdRESH IN (", deps, ");")
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Load indikatorrapport data from",
                                  registryName, ": ", query))
  }

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
getAllYears <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  query <- paste0("
SELECT
  YEAR(RegDato11) as year
FROM
  AlleVarNum
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
  DEPARTMENT_CENTREID IN (", deps, ") AND
  DEPARTMENT_ACTIVE = 1
GROUP BY
  LOCATIONNAME;
                  ")



  # no hospital name for national registry
  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$nationalAccess$reshId) {
    return("Nasjonal")
  } else {
    df <- rapbase::LoadRegData(registryName, dbType = dbType, query = query)
    n <- dim(df)[1]
    hVec <- df[1:n, 1]
    if (n > 1) {
      hStr <- paste(hVec[1:n-1], sep = ", ")
      hStr <- paste(hStr, hVec[n], sep = " og ")
    } else {
      hStr <- paste(hVec)
    }
    return(hStr)
  }
}

#' @rdname getRegData
#' @export
getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {

  # dummy query returning empty data set
  query <- "SELECT * FROM friendlynamestable WHERE 1=0;"

  if (tableName %in% c("friendlynamestable", "change_log_variables",
                       "avdelingsoversikt", "Brukerliste")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, ";
  ")
  }

  if (tableName %in% c("SkjemaOversikt", "SmerteDiagnoser",
                       "SmerteDiagnoserNum", "AlleVar", "AlleVarNum")) {
    query <- paste0("
SELECT
  fo.HovedDato,
  d.*
FROM
  ", tableName, " AS d
LEFT JOIN
  ForlopsOversikt fo
ON
  d.ForlopsID = fo.ForlopsID
WHERE
  fo.HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if (tableName %in% c("ForlopsOversikt")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Smerte data dump:\n", query))
  }
  rapbase::LoadRegData(registryName, query)
}
