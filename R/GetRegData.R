#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param startDate String defing start of date range as YYYY-MM-DD
#' @param endDate String defing end of date range as YYYY-MM-DD
#' @param reshId String providing organization Id
#' @param userRole String providing user role
#' @param tableName String providing a table name
#' @param fromDate String providing start date
#' @param toDate String provideing end date
#' @param asNamedList Logical whether to return a list of named values or not.
#' @param smerteKat Integer defining the SmerteKat code to use in query
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataLokalTilsynsrapportMaaned
#' getRegDataRapportDekningsgrad getRegDataSmertekategori
#' getSmerteDiagKatValueLab
#' getRegDataSpinalkateter getLocalYears getAllYears getHospitalName
#' getNameReshId getDataDump
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
                                                startDate, endDate, ...) {

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
  var.RegDato11 >= DATE('"

  query <- paste0(query, startDate, "') AND var.RegDato11 <= DATE('",
                  endDate, "') AND var.AvdRESH IN (", deps, ");")


  if ("session" %in% names(list(...))) {
    session <- list(...)[["session"]]
    if ("ShinySession" %in% attr(session, "class")) {
      rapbase::repLogger(session = session,
                         msg = paste("Load tilsynsrapport data from",
                                     registryName, ": ", query))
    }
  }

  rapbase::loadRegData(registryName, query, dbType)
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
  SkriftligSamtyk,
  Reservasjonsstatus,
  InklusjonStatus
FROM
  AlleVarNum
WHERE
  AvdRESH IN ("

  query <- paste0(query, deps, ") AND (DATE(StartdatoTO) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                      msg = paste0("Load data from ", registryName, ":", query))
  }

  rapbase::loadRegData(registryName, query, dbType)
}

#' @rdname getRegData
#' @export
getRegDataRapportDekningsgradReservasjon <- function(registryName, reshId, userRole,
                                          startDate, endDate, ...) {
  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- "
SELECT
  PasientID,
  ForlopsID,
  InklKritOppf,
  SkriftligSamtyk,
  Reservasjonsstatus,
  InklusjonStatus
FROM
  AlleVarNum
WHERE
  AvdRESH IN ("

  query <- paste0(query, deps, ") AND (DATE(StartdatoTO) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = paste0("Load data from ", registryName, ":", query))
  }

  rapbase::loadRegData(registryName, query, dbType)
}

#' @rdname getRegData
#' @export
getRegDataIndikator <- function(registryName, reshId, userRole,
                                startDate, endDate, ...) {

  dbType <- "mysql"

  # special case at OUS
  deps <- .getDeps(reshId, userRole)

  query <- paste0("
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
  var.InnlAvd,
  var.VidereOppf,
  var.BehNedtrappAvsluttTils,
  var.Journalnotat,
  var.IkkeMedBeh,
  var.AkseptabelSmerte12,
  var.AkseptabelSmerte21,
  var.Funksjon12,
  var.Funksjon21,
  var.AngiNRS12,
  var.AngiNRS21
FROM
  AlleVarNum var
WHERE
  var.RegDato11>=DATE('", startDate, "') AND var.RegDato11<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- paste0(query, ";")
  } else {
    query <- paste0(query, " AND var.AvdRESH IN (", deps, ");")
  }

  if ("session" %in% names(list(...))) {
    session <- list(...)[["session"]]
    if ("ShinySession" %in% attr(session, "class")) {
      rapbase::repLogger(session = session,
                         msg = paste("Load indikatorrapport data from",
                                     registryName, ": ", query))
    }
  }

  rapbase::loadRegData(registryName, query, dbType)
}

#' @rdname getRegData
#' @export
getRegDataOpiodReduksjon <- function(registryName, reshId, userRole,
                                startDate, endDate, ...) {

  dbType <- "mysql"

  # special case at OUS
  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  var.MoEkvivalens22,
  var.SykehusNavn,
  var.StartdatoTO,
  var.RegDato11
FROM
  AlleVarNum var
WHERE
  var.RegDato11>=DATE('", startDate, "') AND var.RegDato11<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- paste0(query, ";")
  } else {
    query <- paste0(query, " AND var.AvdRESH IN (", deps, ");")
  }

  if ("session" %in% names(list(...))) {
    session <- list(...)[["session"]]
    if ("ShinySession" %in% attr(session, "class")) {
      rapbase::repLogger(session = session,
                         msg = paste("Load opiodrapport data from",
                                     registryName, ": ", query))
    }
  }

  rapbase::loadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getSmerteDiagKatValueLab <- function(registryName, smerteKat) {

  query <- paste0("
SELECT
  val.DiagKat AS value,
  lab.DiagKat AS lable
FROM
  SmerteDiagnoserNum AS val
LEFT JOIN
  SmerteDiagnoser AS lab ON val.SmerteDiagID = lab.SmerteDiagID
WHERE
  val.SmerteKat = ", smerteKat, "
GROUP BY
  val.DiagKat,
  lab.DiagKat;"
  )

  res <- rapbase::loadRegData(registryName, query)

  as.list(stats::setNames(res$lable, res$value))
}

#' @rdname getRegData
#' @export
getRegDataSmertekategori <- function(registryName, reshId, userRole,
                                     startDate, endDate, ...) {
  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- "
SELECT
  diag.ForlopsID,
  fo.HovedDato,
  diag.SmerteDiagID,
  diag.SmerteKat,
  var.AkuttLang,
  diag.DiagKat,
  var.Opioid4a
FROM
  SmerteDiagnoserNum AS diag
LEFT JOIN
  AlleVarNum AS var
ON
  diag.ForlopsID = var.ForlopsID
LEFT JOIN
  ForlopsOversikt AS fo
ON
  diag.ForlopsID = fo.ForlopsID
WHERE
  var.AvdRESH IN ("

  query <- paste0(query, deps, ") AND (DATE(StartdatoTO) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = paste0("Load data from ", registryName, ":", query))
  }

  rapbase::loadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getRegDataSpinalkateter <- function(registryName, reshId, userRole,
                                    startDate, endDate, ...) {

  dbType <- "mysql"

  # special case at OUS
  deps <- .getDeps(reshId, userRole)

  query <- "
SELECT
  MoEkvivalens,
  MoEkvivalens22,
  AntPasTils,
  AntTilsLege,
  AntTilsSykPleier,
  AntTilsFysioT,
  AntTilsPsyk,
  AntTilsSosio,
  StartdatoTO,
  ForlopsID,
  StSmBev12,
  StSmBev21,
  SvSmBev12,
  SvSmBev21,
  StSmRo12,
  StSmRo21,
  SvSmRo12,
  SvSmRo21,
  SAB11,
  PasientID,
  TotTid,
  SluttDato
FROM
  AlleVarNum
WHERE
  AvdRESH IN ("

  query <- paste0(query, deps, ") AND (DATE(StartdatoTO) BETWEEN '",
                  startDate, "' AND '", endDate, "');")

  if ("session" %in% names(list(...))) {
    session <- list(...)[["session"]]
    if ("ShinySession" %in% attr(session, "class")) {
      rapbase::repLogger(session = session,
                         msg = paste("Load spinalkateter data from",
                                     registryName, ": ", query))
    }
  }

  rapbase::loadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getRegDataNRS <- function(registryName, reshId, userRole,
                          startDate, endDate, ...) {

  dbType <- "mysql"

  # special case at OUS
  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  var.StartdatoTO,
  var.ForlopsID,
  var.StSmBev12,
  var.StSmBev21,
  var.SvSmBev12,
  var.SvSmBev21,
  var.StSmRo12,
  var.StSmRo21,
  var.SvSmRo12,
  var.SvSmRo21,
  var.PasientID,
  var.ForlopsID,
  var.Tilsett,
  var.SykehusNavn,
  var.AngiNRS12,
  var.AngiNRS21,
  var.RegDato11
FROM
  AlleVarNum var
WHERE
  var.RegDato11>=DATE('", startDate, "') AND var.RegDato11<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- paste0(query, ";")
  } else {
    query <- paste0(query, " AND var.AvdRESH IN (", deps, ");")
  }

  if ("session" %in% names(list(...))) {
    session <- list(...)[["session"]]
    if ("ShinySession" %in% attr(session, "class")) {
      rapbase::repLogger(session = session,
                         msg = paste("Load opiodrapport data from",
                                     registryName, ": ", query))
    }
  }

  rapbase::loadRegData(registryName, query, dbType)
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

  rapbase::loadRegData(registryName, query, dbType)
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

  rapbase::loadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getHospitalName <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  LOCATION_SHORTNAME AS ln
FROM
  avdelingsoversikt
WHERE
  DEPARTMENT_CENTREID IN (", deps, ") AND
  DEPARTMENT_ACTIVE = 1
GROUP BY
  LOCATION_SHORTNAME;")



  # no hospital name for national registry
  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$nationalAccess$reshId) {
    return("Nasjonal")
  } else {
    df <- rapbase::loadRegData(registryName, dbType = dbType, query = query)
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
getNameReshId <- function(registryName, asNamedList = FALSE) {

  query <- "
SELECT
  SykehusNavn AS name,
  AvdRESH AS id
FROM
  AlleVar
GROUP BY
  SykehusNavn,
  AvdRESH;"

  res <- rapbase::loadRegData(registryName, query)

  if (asNamedList) {
    res <- stats::setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
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
    rapbase::repLogger(session = list(...)[["session"]],
                      msg = paste("Smerte data dump:\n", query))
  }
  rapbase::loadRegData(registryName, query)
}
