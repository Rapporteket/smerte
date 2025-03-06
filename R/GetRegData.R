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
  var.AntTilsKonfLege,
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
  allevarnum var
LEFT JOIN
  avdelingsoversikt avd
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  var.StartdatoTO >= DATE('"

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
    # query <- gsub("avdelingsoversikt", "avdelingsoversiktnasjonal", query)
  }

  query <- paste0(query, startDate, "') AND var.StartdatoTO <= DATE('",
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
  allevarnum
WHERE
  AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

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
  allevarnum
WHERE
  AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

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
  allevarnum var
WHERE
  var.StartdatoTO>=DATE('", startDate, "') AND var.StartdatoTO<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
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
  allevarnum var
WHERE
  var.StartdatoTO>=DATE('", startDate, "') AND var.StartdatoTO<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
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
getSmerteDiagKatValueLab <- function(registryName, reshId, smerteKat) {

  query <- paste0("
SELECT
  val.DiagKat AS value,
  lab.DiagKat AS lable
FROM
  smertediagnosernum AS val
LEFT JOIN
  smertediagnoser AS lab ON val.SmerteDiagID = lab.SmerteDiagID
WHERE
  val.SmerteKat = ", smerteKat, "
GROUP BY
  val.DiagKat,
  lab.DiagKat;"
  )

  if (isNationalReg(reshId)) {
    query <- gsub("smertediagnosernum", "smertediagnosernumnasjonal", query)
    query <- gsub("smertediagnoser", "smertediagnosernasjonal", query)
  }

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
  smertediagnosernum AS diag
LEFT JOIN
  allevarnum AS var
ON
  diag.ForlopsID = var.ForlopsID
LEFT JOIN
  forlopsoversikt AS fo
ON
  diag.ForlopsID = fo.ForlopsID
WHERE
  var.AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("smertediagnosernum", "smertediagnosernumnasjonal", query)
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
    query <- gsub("forlopsoversikt", "forlopsoversiktnasjonal", query)
  }

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
getRegDataTimetodeath <- function(registryName, reshId, userRole,
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
  var.Ddato,
  var.Tilsett,
  var.SluttDato,
  diag.DiagKat,
  var.Opioid4a
FROM
  smertediagnosernum AS diag
LEFT JOIN
  allevarnum AS var
ON
  diag.ForlopsID = var.ForlopsID
LEFT JOIN
  forlopsoversikt AS fo
ON
  diag.ForlopsID = fo.ForlopsID
WHERE
  var.AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("smertediagnosernum", "smertediagnosernumnasjonal", query)
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
    query <- gsub("forlopsoversikt", "forlopsoversiktnasjonal", query)
  }

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
  Opbehd221d,
  LAbehd221d,
  KoAbedel221d,
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
  SA,
  PasientID,
  TotTid,
  SluttDato
FROM
  allevarnum
WHERE
  AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

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
getRegDataLokalEpidural <- function(registryName, reshId, userRole,
                                    startDate, endDate, ...) {
  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- "
SELECT
  PasientID,
  ForlopsID,
  alder,
  EDAB11,
  EDA,
  StartdatoTO
FROM
  allevarnum
WHERE
  AvdRESH IN ("

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

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
getRegDataRapportOppfolg <- function(registryName, reshId, userRole,
                                     startDate, endDate, ...) {

  dbType <- "mysql"

  # special case at OUS
  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  var.Tilsett,
  var.RegDato11,
  var.StartdatoTO,
  var.HenvistDato,
  var.SykehusNavn,
  var.OppfSmeKl,
  var.PasientID,
  var.ForlopsID,
  var.InnlAvd,
  var.VidereOppf,
  avd.DEPARTMENT_ID,
  avd.DEPARTMENT_NAME,
  avd.DEPARTMENT_SHORTNAME
FROM
  allevarnum var
LEFT JOIN
  avdelingsoversikt avd
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  var.StartdatoTO>=DATE('", startDate, "') AND var.StartdatoTO<=DATE('", endDate, "')"
  )

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
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
getLocalYears <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT
  YEAR(StartdatoTO) as year
FROM
  allevarnum
WHERE
  AvdRESH IN (", deps, ")
GROUP BY
  YEAR(StartdatoTO);
")

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

  rapbase::loadRegData(registryName, query, dbType)
}

#' @rdname getRegData
#' @export
getAllYears <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  query <- paste0("
SELECT
  YEAR(StartdatoTO) as year
FROM
  allevarnum
GROUP BY
  YEAR(StartdatoTO);
")

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }

  rapbase::loadRegData(registryName, query, dbType)
}


#' @rdname getRegData
#' @export
getHospitalName <- function(registryName, reshId, userRole) {

  dbType <- "mysql"

  deps <- .getDeps(reshId, userRole)

  query <- paste0("
SELECT DISTINCT
  avd.LOCATION_SHORTNAME AS ln
FROM
  avdelingsoversikt avd
INNER JOIN
  allevarnum var
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  avd.DEPARTMENT_CENTREID IN (", deps, ") AND
  avd.DEPARTMENT_ACTIVE = 1;")

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
  }


  # no hospital name for national registry
  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$nationalAccess$reshId) {
    return("Nasjonal")
  } else {
    df <- rapbase::loadRegData(registryName, dbType = dbType, query = query)
    n <- dim(df)[1]
    hVec <- df[1:n, 1]
    if (n > 1) {
      hStr <- paste(hVec[1:n-1], collapse = ", ")
      hStr <- paste0(hStr, " og ", hVec[n])
    } else {
      hStr <- paste(hVec)
    }
    return(hStr)
  }
}


#' @rdname getRegData
#' @export
getNameReshId <- function(registryName, reshId, asNamedList = FALSE) {

  query <- "
SELECT
  SykehusNavn AS name,
  AvdRESH AS id
FROM
  allevar
GROUP BY
  SykehusNavn,
  AvdRESH;"

  if (isNationalReg(reshId)) {
    query <- gsub("allevar", "allevarnasjonal", query)
  }

  res <- rapbase::loadRegData(registryName, query)

  if (asNamedList) {
    res <- stats::setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
}


#' @rdname getRegData
#' @export
getDataDump <- function(registryName, reshId, tableName, fromDate, toDate, ...) {

  # dummy query returning empty data set
  query <- "SELECT * FROM avdelingsoversikt WHERE 1=0;"

  if (tableName %in% c("friendlynamestable", "change_log_variables",
                       "avdelingsoversikt", "Brukerliste")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, ";
  ")
  }

  if (tableName %in% c("skjemaoversikt", "smertediagnoser",
                       "smertediagnosernum", "allevar", "allevarnum",
                       "smertediagnosernumnasjonal", "allevarnasjonal",
                       "allevarnumnasjonal"
                       )) {
    query <- paste0("
SELECT
  fo.HovedDato,
  d.*
FROM
  ", tableName, " AS d
LEFT JOIN
  forlopsoversikt fo
ON
  d.ForlopsID = fo.ForlopsID
WHERE
  fo.HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if (isNationalReg(reshId)) {
    query <- gsub("forlopsoversikt", "forlopsoversiktnasjonal", query)
  }

  if (tableName %in% c("forlopsoversikt", "forlopsoversiktnasjonal")) {
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
