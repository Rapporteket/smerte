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

#' getListTextFunction
#'
#' @description Hjelpefunksjon for å trekke ut kategoriske labels fra
#' 'text'-tabell for smerteregisteret. Brukes per nå i uttrekk 'smertediagnoser'.
#' Åpner kobling mot database via rapbase-funksjoner. Kjøres som del av
#' datauttrekk til Rapporteket. Ikke ment å kalles direkte.
#'
#' @param registryName Databasekobling.
#'
#' @export
#'
#' @keywords internal
getListTextFunction = function(registryName) {
  con = rapbase::rapOpenDbConnection(dbName = registryName, dbType = "mysql")
  dbExecute(con$con, "DROP FUNCTION IF EXISTS getListText")
  dbExecute(con$con,
            "
  CREATE FUNCTION getListText(
    p_list_name VARCHAR(255),
    p_code INT
  ) RETURNS VARCHAR(4000) CHARSET utf8mb4
  DETERMINISTIC
  READS SQL DATA
  BEGIN
    DECLARE v_text VARCHAR(4000);
    SELECT TEXT INTO v_text
    FROM text
    WHERE ID = CONCAT(p_list_name, '_L_', p_code, '_D')
      AND LANGUAGEID = 'no'
    LIMIT 1;
    RETURN COALESCE(v_text, CONCAT('Unknown: ', p_code));
  END
  ")
  rapbase::rapCloseDbConnection(con$con)
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
  if (isNationalReg(reshId)) {
    query <- gsub("var.AvdRESH IN (0) AND", "", query, fixed = TRUE)
  }

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
  allevarnum
GROUP BY
  SykehusNavn,
  AvdRESH;"

  if (isNationalReg(reshId)) {
    query <- gsub("allevarnum", "allevarnumnasjonal", query)
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
getDataDump <- function(registryName, reshId, userRole, tableName, fromDate, toDate, ...) {

  # Liste over tabeller som skal være tilgjengelig for uttrekk
  rådatatabeller = c("patient", "emp11", "emp_11_pain_diagnosis",
                     "emp12", "emp22", "hads",
                     "mce", "opiodoppf", "pateval", "patreg")

  koblet = c("allevarnum", "smertediagnosernum", "smertediagnoser")

  if(!tableName %in% c(rådatatabeller, koblet)) {
    stop(message = "Ukjent datasett")
  }


  # Filtrere for resh og valgte datoer
  if (reshId == 0) {
    userInput = paste0("WHERE mce.REGISTERED_DATE BETWEEN
    CAST('", fromDate, "' AS DATE) AND CAST('", toDate, "' AS DATE)")
  } else {
    userInput = paste0("WHERE mce.CENTREID IN (", .getDeps(reshId = reshId, userRole = userRole) , ") AND
    mce.REGISTERED_DATE BETWEEN
    CAST('", fromDate, "' AS DATE) AND CAST('", toDate, "' AS DATE)")
  }

  # Lage spørringer
  if(tableName %in% rådatatabeller) {

    query = paste0("SELECT tab.*
                 FROM mce mce
                    INNER JOIN ", tableName, " tab ON
                    COALESCE(NULLIF(mce.PARENT_ID, 'NA'), mce.MCEID) = tab.MCEID ",
                   userInput
    )
  } else {
    query = bygg_query(tableName, userInput)
  }

  # LOGGING
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = paste0("Smerte data dump: ", tableName, "\n Query: ", query))
  }

  # Henter uttrekk
  rapbase::loadRegData(registryName, query)
}

#' bygg_query
#'
#' @description Hjelpefunksjon for å bygge spørringer for å generere koblede
#' uttrekk for smerteregisteret. Kjøres som del av datauttrekk til Rapporteket.
#' Ikke ment å kalles direkte.
#'
#' @param tableName Navn på koblet uttrekk som ønskes. Per nå er det støtte for
#' 'allevarnum', 'smertediagnosernum' og 'smertediagnoser'.
#' @param userInput parametre hentet fra bruker-input i shiny.
#'
#' @returns
#' Returnerer spørring for aktuelt uttrekk.
#' @export
#'
#' @keywords internal
bygg_query = function(tableName, userInput) {

  if(tableName == "allevarnum") {
    query = paste0("SELECT
    mce.PATIENT_ID AS PasientID,
    mce.CENTREID AS AvdRESH,
    -- getFriendlyName(mce.CENTREID) AS SykehusNavn,
    mce.MCEID AS ForlopsID,
    mce.INCLUDED_RAPPORTEKET as InklusjonStatus,
    emp11.REGISTERED_DATE AS RegDato11,
    emp11.LOCATION AS Lokasjon,
    emp11.DEPARTMENT AS InnlAvd,
    emp11.MAINDIAG AS InnlDiag,
    emp11.MAINDIAG_VERSION AS InnlDiagVersj,
    emp11.AKUTTLANGV AS AkuttLang,
    emp11.ANALGESICS_NONE AS Ingen4a,
    emp11.ANALGESICS_NON_OPIOIDS AS NonOpioid4a,
    emp11.ANALGESICS_OPIOIDS AS Opioid4a,
    emp11.ANALGESICS_BENZODIAZEPINES AS Benzo4a,
    emp11.ANALGESICS_COANALGESICS AS KoAnalg4a,
    emp11.ANALGESICS_UNKNOWN AS Vetikke4a,
    emp11.ANALGESICS_FIRST_NONE AS Ingen4b,
    emp11.ANALGESICS_FIRST_NON_OPIOIDS AS NonOpioid4b,
    emp11.ANALGESICS_FIRST_OPIOIDS AS Opioid4b,
    emp11.ANALGESICS_FIRST_BENZODIAZEPINES AS Benzo4b,
    emp11.ANALGESICS_FIRST_COANALGESICS AS KoAnalg4b,
    emp11.ANALGESICS_FIRST_UNKNOWN AS Vetikke4b,
    emp11.OMEQ_MORPHINE_PILLS_DOSAGE AS DDMoTbl,
    emp11.OMEQ_MORPHINE_PILLS_OMEQ AS OMEQMoTbl,
    emp11.OMEQ_MORPHINE_RELEASEPILLS_DOSAGE AS DDMoDep,
    emp11.OMEQ_MORPHINE_RELEASEPILLS_OMEQ AS OMEQMoDep,
    emp11.OMEQ_MORPHINE_INTRAVENOUS_DOSAGE AS DDMoIv,
    emp11.OMEQ_MORPHINE_INTRAVENOUS_OMEQ AS OMEQMoIv,
    emp11.OMEQ_OXYCODONE_CAPSULES_DOSAGE AS DDOxKps,
    emp11.OMEQ_OXYCODONE_CAPSULES_OMEQ AS OMEQOxKps,
    emp11.OMEQ_OXYCODONE_RELEASEPILLS_DOSAGE AS DDOxDep,
    emp11.OMEQ_OXYCODONE_RELEASEPILLS_OMEQ AS OMEQOxDep,
    emp11.OMEQ_OXYCODONE_INTRAVENOUS_DOSAGE AS DDOxIv,
    emp11.OMEQ_OXYCODONE_INTRAVENOUS_OMEQ AS OMEQOxIv,
    emp11.OMEQ_BUVIDAL_WEEKLY_DEPOTINJECTION_DOSAGE AS DDBuvWDepIn,
    emp11.OMEQ_BUVIDAL_WEEKLY_DEPOTINJECTION_OMEQ AS OMEQBuvWDepIn,
    emp11.OMEQ_BUVIDAL_MONTHLY_DEPOTINJECTION_DOSAGE AS DDBuvMDepIn,
    emp11.OMEQ_BUVIDAL_MONTHLY_DEPOTINJECTION_OMEQ AS OMEQBuvMDepIn,
    emp11.OMEQ_BUPRENORPHINE_PATCH_DOSAGE AS DDBuPl,
    emp11.OMEQ_BUPRENORPHINE_PATCH_OMEQ AS OMEQBuPl,
    emp11.OMEQ_BUPRENORPHINE_PILLS_DOSAGE AS DDBuTbl,
    emp11.OMEQ_BUPRENORPHINE_PILLS_OMEQ AS OMEQBuTbl,
    emp11.OMEQ_FENTANYL_PATCH_DOSAGE AS DDFePl,
    emp11.OMEQ_FENTANYL_PATCH_OMEQ AS OMEQFePl,
    emp11.OMEQ_FENTANYL_PILLS_AND_SPRAY_DOSAGE AS DDFeTblSpray,
    emp11.OMEQ_FENTANYL_PILLS_AND_SPRAY_OMEQ AS OMEQFeTblSpray,
    emp11.OMEQ_FENTANYL_IV_DOSAGE AS DDFeIV,
    emp11.OMEQ_FENTANYL_IV_OMEQ AS OMEQFeIV,
    emp11.OMEQ_HYDROMORPHONE_CAPSULES_DOSAGE AS DDHyKps,
    emp11.OMEQ_HYDROMORPHONE_CAPSULES_OMEQ AS OMEQHyKps,
    emp11.OMEQ_HYDROMORPHONE_RELEASECAPSULES_DOSAGE AS DDHyDepKps,
    emp11.OMEQ_HYDROMORPHONE_RELEASECAPSULES_OMEQ AS OMEQHyDepKps,
    emp11.OMEQ_HYDROMORPHONE_INTRAVENOUS_DOSAGE AS DDHyIv,
    emp11.OMEQ_HYDROMORPHONE_INTRAVENOUS_OMEQ AS OMEQHyIv,
    emp11.OMEQ_KETOBEMIDONE_INTRAVENOUS_DOSAGE AS DDKeIv,
    emp11.OMEQ_KETOBEMIDONE_INTRAVENOUS_OMEQ AS OMEQKeIv,
    emp11.OMEQ_KETOBEMIDONE_PILLS_DOSAGE AS DDKeTbl,
    emp11.OMEQ_KETOBEMIDONE_PILLS_OMEQ AS OMEQKeTbl,
    emp11.OMEQ_PETHIDINE_SUPPOSITORIES_DOSAGE AS DDPeSupp,
    emp11.OMEQ_PETHIDINE_SUPPOSITORIES_OMEQ AS OMEQPeSupp,
    emp11.OMEQ_PETHIDINE_INTRAMUSCULAR_DOSAGE AS DDPeIm,
    emp11.OMEQ_PETHIDINE_INTRAMUSCULAR_OMEQ AS OMEQPeIm,
    emp11.OMEQ_CODEINE_PILLS_DOSAGE AS DDCoTbl,
    emp11.OMEQ_CODEINE_PILLS_OMEQ AS OMEQCoTbl,
    emp11.OMEQ_TRAMADOL_CAPSULES_DOSAGE AS DDTrKps,
    emp11.OMEQ_TRAMADOL_CAPSULES_OMEQ AS OMEQTrKps,
    emp11.OMEQ_TRAMADOL_RELEASEPILLS_DOSAGE AS DDTrDepTbl,
    emp11.OMEQ_TRAMADOL_RELEASEPILLS_OMEQ AS OMEQTrDepTbl,
    emp11.OMEQ_TAPENTADOL_RELEASEPILLS_DOSAGE AS DDTaDepTbl,
    emp11.OMEQ_TAPENTADOL_RELEASEPILLS_OMEQ AS OMEQTaDepTbl,
    emp11.OMEQ_TAPENTADOL_PILLS_DOSAGE AS DDTaMiksTbl,
    emp11.OMEQ_TAPENTADOL_PILLS_OMEQ AS OMEQTaMiksTbl,
    emp11.OMEQ_METHADONE_POTION_DOSAGE AS DDMetMikst,
    emp11.OMEQ_METHADONE_POTION_OMEQ AS OMEQMetMikst,
    emp11.OMEQ_METHADONE_INTRAVENOUS_DOSAGE AS DDMetIv,
    emp11.OMEQ_METHADONE_INTRAVENOUS_OMEQ AS OMEQMetIv,
    emp11.OMEQ_MORPHINEEQUIVALENCE AS MoEkvivalens,
    emp11.ANALGETICS_INEXP AS UhenBrukMed,
    emp11.CENTRALBLOCKAGES AS SentralBlokkB11,
    emp11.CENTRALBLOCKAGES_TYPE_EPIDURAL AS EDAB11,
    emp11.CENTRALBLOCKAGES_TYPE_SPINAL AS SAB11,
    emp11.CENTRALBLOCKAGES_MEDS_OPIOIDS AS Opbehd,
    emp11.CENTRALBLOCKAGES_MEDS_LOCAL_ANAESTHESIA AS LAbehd,
    emp11.CENTRALBLOCKAGES_MEDS_CO_ANALGESICS AS KoAbedel,
    emp11.CENTRALBLOCKAGES_MEDS_OTHER AS Annetbehd,
    emp11.PERIPHERALBLOCKAGES AS PeriferBlokkB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PLEXUS_BRACHIALIS AS PlBrachBlokB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_INTERSCALEN AS InterscB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_SUPRACLAVICULAR AS SupraclavB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_INFRACLAVICULAR AS InfraclavB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_AXILUAR AS AxilB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PARAVERTEBRAL AS PVBB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_INTERCOSTAL AS ICBB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PECS AS PECSB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PECS_1 AS PECSUnderkat1B11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PECS_2 AS PECSUnderkat2B11,
    emp11.PERIPHERALBLOCKAGES_TYPE_TAP AS TAPB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_FEMORAL AS FemblokB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_ADDUKTOR AS AdKanalblokB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_ISCHIADICUS AS IschiasblokB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_SUBGLUTEAL AS SubglutB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_POPLITEA AS PopliteaB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_PLEXUS_LUBALIS AS PLBB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_FASCIA_ILIACA AS FICBB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_SERRATURS AS SPBB11,
    emp11.PERIPHERALBLOCKAGES_TYPE_OTHER AS AnnenBlokB11,
    emp11.PERIPHERALBLOCKAGES_METHOD_SINGELSHOT AS SShotB11,
    emp11.PERIPHERALBLOCKAGES_METHOD_INFUSJON AS InfusjonB11,
    emp11.HISTORYOFABUSE AS TidligereRus,
    emp11.CURRENTABUSE AS PaagaaRus,
    emp11.LAR_OR_LAS AS LAR,
    emp11.REG_FREQUENCY AS AvdRegSmIntensSD,
    emp11.REG_FREQUENCY_MISS AS AvdRegSmIntensSDMangler,
    -- NB! Table emp11 has child tables that you need to consider!
    emp12.REGISTERED_DATE AS Regdato12,
    emp12.PATIENT_ALLRIGHT AS AngiNRS12,
    emp12.STRONGPAIN_STILL AS StSmRo12,
    emp12.WEAKPAIN_STILL AS SvSmRo12,
    emp12.STRONGPAIN_MOTION AS StSmBev12,
    emp12.WEAKPAIN_MOTION AS SvSmBev12,
    emp12.PATIENT_ACCEPTABLEPAIN AS AkseptabelSmerte12,
    emp12.PATIENT_FUNCTIONLEVEL AS Funksjon12,
    emp21.REGISTERED_DATE AS Regdato21,
    emp21.PATIENT_ALLRIGHT AS AngiNRS21,
    emp21.STRONGPAIN_STILL AS StSmRo21,
    emp21.WEAKPAIN_STILL AS SvSmRo21,
    emp21.STRONGPAIN_MOTION AS StSmBev21,
    emp21.WEAKPAIN_MOTION AS SvSmBev21,
    emp21.PATIENT_ACCEPTABLEPAIN AS AkseptabelSmerte21,
    emp21.PATIENT_FUNCTIONLEVEL AS Funksjon21,
    emp22.END_DATE AS SluttDato,
    emp22.CAREGIVER_DOCTOR AS Lege,
    emp22.CAREGIVER_DOCTOR_TIMES AS AntTilsLege,
    emp22.CAREGIVER_DOCTOR_ADVICE AS RadBehandlerAST,
    emp22.CAREGIVER_DOCTOR_ADVICE_TIMES AS AntTilsRadBehandlerAST,
    emp22.CAREGIVER_DOCTOR_CONS AS KonfLege,
    emp22.CAREGIVER_DOCTOR_CONS_TIMES AS AntTilsKonfLege,
    emp22.CAREGIVER_NURSE AS SykPleier,
    emp22.CAREGIVER_NURSE_TIMES AS AntTilsSykPleier,
    emp22.CAREGIVER_PHYSIOTHERAPIST AS FysioT,
    emp22.CAREGIVER_PHYSIOTHERAPIST_TIMES AS AntTilsFysioT,
    emp22.CAREGIVER_PSYCHOLOGIST AS Psyk,
    emp22.CAREGIVER_PSYCHOLOGIST_TIMES AS AntTilsPsyk,
    emp22.CAREGIVER_SOCIAL_WORKER AS Sosio,
    emp22.CAREGIVER_SOCIAL_WORKER_TIMES AS AntTilsSosio,
    emp22.CAREGIVER_AMB_DOCTOR AS AmbLege,
    emp22.CAREGIVER_AMB_DOCTOR_TIMES AS AntTilsAmbLege,
    emp22.CAREGIVER_AMB_NURSE AS AmbSykepleier,
    emp22.CAREGIVER_AMB_NURSE_TIMES AS AntTilsAmbSpl,
    emp22.CAREGIVER_PRIEST AS Prest,
    emp22.CAREGIVER_PRIEST_TIMES AS AntTilsPrest,
    emp22.CONSULTATIONS AS AntPasTils,
    emp22.CONSULTATIONS_TIME AS TotTid,
    emp22.INITIATED_BY_TEAM_MEDS AS MedBeh22,
    emp22.RESPIRATION_DEP_INITIATIVES_NONE AS Ingen1a,
    emp22.RESPIRATION_DEP_INITIATIVES_OXYGEN AS O2beh,
    emp22.RESPIRATION_DEP_INITIATIVES_ANTIDOTE AS Motgift1a,
    emp22.RESPIRATION_DEP_INITIATIVES_MONITOR AS ForstOvervaakn,
    emp22.RESPIRATION_DEP_INITIATIVES_INTENSIVE AS IntensivBeh,
    emp22.RESPIRATION_DEP_INITIATIVES_RECUSITATION AS Resuc,
    emp22.RESPIRATION_DEP_INITIATIVES_RESPIRATOR AS RespiratorBeh,
    emp22.INITIATED_BY_TEAM_NON_MEDICAL AS IkkeMedBeh,
    emp22.INITIATED_BY_TEAM_PSYCHOSOCIAL AS PSBeh,
    emp22.INITIATED_BY_TEAM_PHYSIOTHERAPY AS FysioBeh,
    emp22.INITIATED_BY_TEAM_NON_MEDICAL_OTHER AS AnnenIkkeMedBeh,
    emp22.INITIATED_BY_TEAM_CENTRALBLOCKAGES AS SentralBlokk,
    emp22.CENTRALBLOCKAGES_TYPE_EPIDURAL AS EDA,
    emp22.CENTRALBLOCKAGES_TYPE_SPINAL AS SA,
    emp22.CENTRALBLOCKAGES_MEDS_OPIOIDS AS Opbehd221d,
    emp22.CENTRALBLOCKAGES_MEDS_LOCAL_ANAESTHESIA AS LAbehd221d,
    emp22.CENTRALBLOCKAGES_MEDS_CO_ANALGESICS AS KoAbedel221d,
    emp22.CENTRALBLOCKAGES_MEDS_OTHER AS Annetbehd221d,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS AS Kompl1d,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_HEMATOME AS ISEDHematom,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_MENINGITIS AS Meningitt,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_ABCESS AS EDabsc,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_NERVES AS NervSkad1d,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_CATHETER AS MigKat,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_LOCALANASTHESIA AS IvinjLA,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_HEADACHE AS PShodepine,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_FALLINGTENDENCY AS FallpgaLA,
    emp22.CENTRALBLOCKAGES_COMPLICATIONS_CSF_LEAK AS LekCSF,
    emp22.CENTRALBLOCKAGES_INITIATIVES_NONE AS Ingen1d,
    emp22.CENTRALBLOCKAGES_INITIATIVES_OPERATION AS Operasjon1d,
    emp22.CENTRALBLOCKAGES_INITIATIVES_ANTIBIOTICS AS AbBeh1d,
    emp22.CENTRALBLOCKAGES_INITIATIVES_CATHETERREMOVAL AS FjernKat1d,
    emp22.CENTRALBLOCKAGES_INITIATIVES_BLOODPATCH AS Bloodpatch,
    emp22.CENTRALBLOCKAGES_INITIATIVES_BEDREST AS Sengeleie,
    emp22.CENTRALBLOCKAGES_INITIATIVES_FENAZON AS FzKoff,
    emp22.CENTRALBLOCKAGES_INITIATIVES_CHANGED_MEDICATION AS EndrMed,
    emp22.CENTRALBLOCKAGES_INITIATIVES_OTHER AS Annet1d,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES AS PeriferBlokk,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PLEXUS_BRACHIALIS AS PlBrachBlok,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_INTERSCALEN AS Intersc,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_SUPRACLAVICULAR AS Supraclav,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_INFRACLAVICULAR AS Infraklav,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_AXILUAR AS Axil,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PARAVERTEBRAL AS PVB,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_INTERCOSTAL AS ICB,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PECS AS PECS,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PECS_1 AS Underkat1,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PECS_2 AS Underkat2,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_TAP AS TAP,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_FEMORAL AS Femblok,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_ADDUKTOR AS AdKanalblok,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_ISCHIADICUS AS Ischiasblok,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_SUBGLUTEAL AS Subglut,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_POPLITEA AS Popliteal,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_PLEXUS_LUBALIS AS PLB,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_FASCIA_ILIACA AS FICB,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_SERRATURS AS SPB,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_TYPE_OTHER AS AnnenBlok,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_METHOD_SINGELSHOT AS SShot,
    emp22.INITIATED_BY_TEAM_PERIPHERALBLOCKAGES_METHOD_INFUSJON AS Infusjon,
    emp22.PERIPHERALBLOCKAGES_COMPLICATIONS AS Kompl1e,
    emp22.PERIPHERALBLOCKAGES_COMPLICATIONS_NERVES AS NervSkad1e,
    emp22.PERIPHERALBLOCKAGES_COMPLICATIONS_INFECTION AS Infeksjon1e,
    emp22.PERIPHERALBLOCKAGES_COMPLICATIONS_ANESTHESIA AS IvInjLA1e,
    emp22.PERIPHERALBLOCKAGES_COMPLICATIONS_INTOXICATION AS Intox1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_NONE AS Ingen1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_OPERATION AS Op1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_ANTIBIOTICS AS AbBeh1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_CATHETERREMOVAL AS FjernKat1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_CHANGEDMEDICATION AS EndretMedInt1e,
    emp22.PERIPHERALBLOCKAGES_INITIATIVES_OTHER AS Annet1e,
    emp22.SHOULD_FOLLOWUP AS VidereOppf,
    emp22.DOWNSCALING AS BehNedtrappAvsluttTils,
    emp22.DOWNSCALINGMEASURES_JOURNAL AS Journalnotat,
    emp22.DOWNSCALINGMEASURES_PATIENT AS PlanLevertPas,
    emp22.DOWNSCALINGMEASURES_BROCHURE AS UtdeltOpioidbrosjyre,
    emp22.DOWNSCALINGMEASURES_WARD AS OppfolgAvdeling,
    emp22.NO_DOWNSCALING_REASON AS AarsakVedNei,
    emp22.CONTACTED_OTHER AS OppfanneInst,
    emp22.CONTACTED AS KontFastlege,
    emp22.PAIN_CLINIC AS OppfSmeKl,
    emp22.USERCOMMENT AS IntKom22,
    emp22.STATUS AS FerdigSkjema22,
    emp22.INITIATED_BY_TEAM_MEDS_OPIOIDS AS Op22,
    emp22.INITIATED_BY_TEAM_MEDS_NON_OPIOIDS AS NOp22,
    emp22.INITIATED_BY_TEAM_MEDS_CO_ANALGESICS AS KoAn22,
    emp22.INITIATED_BY_TEAM_MEDS_BENZO AS Bzd22,
    emp22.OMEQ_MORPHINE_PILLS_DOSAGE AS DDMoTbl22,
    emp22.OMEQ_MORPHINE_PILLS_OMEQ AS OMEQMoTbl22,
    emp22.OMEQ_MORPHINE_RELEASEPILLS_DOSAGE AS DDMoDep22,
    emp22.OMEQ_MORPHINE_RELEASEPILLS_OMEQ AS OMEQMoDep22,
    emp22.OMEQ_MORPHINE_INTRAVENOUS_DOSAGE AS DDMoIv22,
    emp22.OMEQ_MORPHINE_INTRAVENOUS_OMEQ AS OMEQMoIv22,
    emp22.OMEQ_OXYCODONE_CAPSULES_DOSAGE AS DDOxKps22,
    emp22.OMEQ_OXYCODONE_CAPSULES_OMEQ AS OMEQOxKps22,
    emp22.OMEQ_OXYCODONE_RELEASEPILLS_DOSAGE AS DDOxDep22,
    emp22.OMEQ_OXYCODONE_RELEASEPILLS_OMEQ AS OMEQOxDep22,
    emp22.OMEQ_OXYCODONE_INTRAVENOUS_DOSAGE AS DDOxIv22,
    emp22.OMEQ_OXYCODONE_INTRAVENOUS_OMEQ AS OMEQOxIv22,
    emp22.OMEQ_BUVIDAL_WEEKLY_DEPOTINJECTION_DOSAGE AS DDBuvWDepIn22,
    emp22.OMEQ_BUVIDAL_WEEKLY_DEPOTINJECTION_OMEQ AS OMEQBuvWDepIn22,
    emp22.OMEQ_BUVIDAL_MONTHLY_DEPOTINJECTION_DOSAGE AS DDBuvMDepIn22,
    emp22.OMEQ_BUVIDAL_MONTHLY_DEPOTINJECTION_OMEQ AS OMEQBuvMDepIn22,
    emp22.OMEQ_BUPRENORPHINE_PATCH_DOSAGE AS DDBuPl22,
    emp22.OMEQ_BUPRENORPHINE_PATCH_OMEQ AS OMEQBuPl22,
    emp22.OMEQ_BUPRENORPHINE_PILLS_DOSAGE AS DDBuTbl22,
    emp22.OMEQ_BUPRENORPHINE_PILLS_OMEQ AS OMEQBuTbl22,
    emp22.OMEQ_FENTANYL_PATCH_DOSAGE AS DDFePl22,
    emp22.OMEQ_FENTANYL_PATCH_OMEQ AS OMEQFePl22,
    emp22.OMEQ_FENTANYL_PILLS_AND_SPRAY_DOSAGE AS DDFeTblSpray22,
    emp22.OMEQ_FENTANYL_PILLS_AND_SPRAY_OMEQ AS OMEQFeTblSpray22,
    emp22.OMEQ_FENTANYL_IV_DOSAGE AS DDFeIV22,
    emp22.OMEQ_FENTANYL_IV_OMEQ AS OMEQFeIV22,
    emp22.OMEQ_HYDROMORPHONE_CAPSULES_DOSAGE AS DDHyKps22,
    emp22.OMEQ_HYDROMORPHONE_CAPSULES_OMEQ AS OMEQHyKps22,
    emp22.OMEQ_HYDROMORPHONE_RELEASECAPSULES_DOSAGE AS DDHyDepKps22,
    emp22.OMEQ_HYDROMORPHONE_RELEASECAPSULES_OMEQ AS OMEQHyDepKps22,
    emp22.OMEQ_HYDROMORPHONE_INTRAVENOUS_DOSAGE AS DDHyIv22,
    emp22.OMEQ_HYDROMORPHONE_INTRAVENOUS_OMEQ AS OMEQHyIv22,
    emp22.OMEQ_KETOBEMIDONE_INTRAVENOUS_DOSAGE AS DDKeIv22,
    emp22.OMEQ_KETOBEMIDONE_INTRAVENOUS_OMEQ AS OMEQKeIv22,
    emp22.OMEQ_KETOBEMIDONE_PILLS_DOSAGE AS DDKeTbl22,
    emp22.OMEQ_KETOBEMIDONE_PILLS_OMEQ AS OMEQKeTbl22,
    emp22.OMEQ_PETHIDINE_SUPPOSITORIES_DOSAGE AS DDPeSupp22,
    emp22.OMEQ_PETHIDINE_SUPPOSITORIES_OMEQ AS OMEQPeSupp22,
    emp22.OMEQ_PETHIDINE_INTRAMUSCULAR_DOSAGE AS DDPeIm22,
    emp22.OMEQ_PETHIDINE_INTRAMUSCULAR_OMEQ AS OMEQPeIm22,
    emp22.OMEQ_CODEINE_PILLS_DOSAGE AS DDCoTbl22,
    emp22.OMEQ_CODEINE_PILLS_OMEQ AS OMEQCoTbl22,
    emp22.OMEQ_TRAMADOL_CAPSULES_DOSAGE AS DDTrKps22,
    emp22.OMEQ_TRAMADOL_CAPSULES_OMEQ AS OMEQTrKps22,
    emp22.OMEQ_TRAMADOL_RELEASEPILLS_DOSAGE AS DDTrDepTbl22,
    emp22.OMEQ_TRAMADOL_RELEASEPILLS_OMEQ AS OMEQTrDepTbl22,
    emp22.OMEQ_TAPENTADOL_RELEASEPILLS_DOSAGE AS DDTaDepTbl22,
    emp22.OMEQ_TAPENTADOL_RELEASEPILLS_OMEQ AS OMEQTaDepTbl22,
    emp22.OMEQ_TAPENTADOL_PILLS_DOSAGE AS DDTaMiksTbl22,
    emp22.OMEQ_TAPENTADOL_PILLS_OMEQ AS OMEQTaMiksTbl22,
    emp22.OMEQ_METHADONE_POTION_DOSAGE AS DDMetMikst22,
    emp22.OMEQ_METHADONE_POTION_OMEQ AS OMEQMetMikst22,
    emp22.OMEQ_METHADONE_INTRAVENOUS_DOSAGE AS DDMetIv22,
    emp22.OMEQ_METHADONE_INTRAVENOUS_OMEQ AS OMEQMetIv22,
    emp22.OMEQ_MORPHINEEQUIVALENCE AS MoEkvivalens22,
    emp22.RESPIRATION_DEP AS AlvRespDepr,
    hads.REGISTERED_DATE AS DatoUtfHADS,
    hads.COMPLETE AS PasDelUtf,
    hads.INCOMPLETE_REASON AS GrManglUtfylHADS,
    hads.Q1 AS HADSSpm1,
    hads.Q2 AS HADSSpm2,
    hads.Q3 AS HADSSpm3,
    hads.Q4 AS HADSSpm4,
    hads.Q5 AS HADSSpm5,
    hads.Q6 AS HADSSpm6,
    hads.Q7 AS HADSSpm7,
    hads.Q8 AS HADSSpm8,
    hads.Q9 AS HADSSpm9,
    hads.Q10 AS HADSSpm10,
    hads.Q11 AS HADSSpm11,
    hads.Q12 AS HADSSpm12,
    hads.Q13 AS HADSSpm13,
    hads.Q14 AS HADSSpm14,
    hads.DEPRESSIONSCORE AS HADSScoreDepresjon,
    hads.ANXIETYSCORE AS HADSScoreAngst,
    mce.REFERRAL_DATE AS HenvistDato,
    mce.REFERRED_BY_REQUEST AS Tilsynsanmodnin,
    mce.REFERRED_BY_PHONE AS TelefonCalling,
    mce.REGISTERED_DATE AS StartdatoTO,
    mce.SUPERVISION AS Tilsett,
    mce.NORWEGIAN AS Norsktalende,
    mce.AGE AS alder,
    mce.COGNITIVE AS KognSvekket,
    mce.INCLUDED AS InklKritOppf,
    mce.CONSENT AS SkriftligSamtyk,
    mce.NOCONSENT_REASON AS TypeManglSamt,
    mce.CONSENT_DATE AS DatoSamtykke,
    mce.CONSENT_WITHDRAW_DATE AS DatotrekSamt,
    opioidoppf.REGISTERED_DATE AS DatoUtfylOpioid,
    opioidoppf.COMPLETE AS PasdelUtfylOpioid,
    opioidoppf.INCOMPLETE_REASON AS AarsakmanglUtfylOpioid,
    opioidoppf.DISCHARGED_HOSPITAL AS Utskrevetsykehus,
    opioidoppf.MEDICATION_DISCHARGED AS Smstilletterutskrivelse,
    opioidoppf.MEDICIN_DOLCONTIN AS DolcontinMalfin,
    opioidoppf.MEDICIN_FENTANYLPLASTER AS FentanylplDurogesicpl,
    opioidoppf.MEDICIN_METADON AS Metadon,
    opioidoppf.MEDICIN_MORFIN AS Morfin,
    opioidoppf.MEDICIN_NORSPAN_PLASTER AS Norspanplaster,
    opioidoppf.MEDICIN_OXYCONTIN AS OxyxontinReltebon,
    opioidoppf.MEDICIN_OXYNORM AS OxynormOxykodon,
    opioidoppf.MEDICIN_PALEXIA AS Palexia,
    opioidoppf.MEDICIN_PALLADON AS PalladonHydromorfon,
    opioidoppf.MEDICIN_PARALGIN_FORTE AS Paralginforte,
    opioidoppf.MEDICIN_SUBUTEX AS Buprenorfin,
    opioidoppf.MEDICIN_TARGINIQ AS Targiniq,
    opioidoppf.MEDICIN_TRAMADOL AS Tramadolvarianter,
    opioidoppf.MEDICIN_OTHER AS Andre,
    opioidoppf.MEDICIN_USIKKER AS Usikker,
    opioidoppf.MEDICATION_IN_USE AS Fortsattbruk,
    opioidoppf.INFO_MED_REDUCING AS InfoNedtrapping,
    opioidoppf.INFO_MED_REDUCING_BY_WHOM AS HvemInfoNedtrapping,
    opioidoppf.INFO_MED_REDUCING_METHOD_YES_ORAL AS MuntligInfoNedtrapp,
    opioidoppf.INFO_MED_REDUCING_METHOD_YES_TEXT_PLAN AS SkriftligNedtrappingsplan,
    opioidoppf.INFO_MED_REDUCING_METHOD_YES_PAPER AS InfoBrosjyreNedtrapp,
    opioidoppf.FOLLOW_MED_RECOMMENDATION AS FolgeAnbefalingNedtrapp,
    opioidoppf.CHALLENGES_DOWNSCALE_ABSTINENCE AS OpplevdAbstinenssympt,
    opioidoppf.REALIZED_ABSTINENCE_ABSTINENCE AS ForstodAbstinenssympt,
    opioidoppf.KNOW_WHAT_TO_DO_ABSTINENCE_ABSTINENCE AS LosningProblemAbstinens,
    opioidoppf.CHALLENGES_DOWNSCALE_REALIZED AS OpplevdPlagsomTretthet,
    opioidoppf.KNOW_WHAT_TO_DO_DOWNSCALE AS LosningProblemTretthet,
    opioidoppf.INFO_MED_DRIVING AS InfoSmertestillBilkjoring,
    opioidoppf.INFO_MED_DRIVING_BY_WHOM AS HvemInfoSmertestillBilkjoring,
    opioidoppf.INFO_METHOD_MED_DRIVING_ORAL AS MuntligInfoBilkjoring,
    opioidoppf.INFO_METHOD_MED_DRIVING_WRITTEN AS SkriftigInfoBilkjoring,
    opioidoppf.INFO_MED_REDUCING_YES_PAPER3 AS InfoBrosyjreBilkjoring,
    opioidoppf.CONTACT_DOWNSCALE AS HenvendelseAngSmertestill,
    opioidoppf.NEED_INFO_DOWNSCALE AS InfoBrukNedtrapp,
    opioidoppf.NEED_INFO_DRIVING AS InfoSmstillBilkjoring,
    opioidoppf.NEED_INFO_ABSTINENCE AS InfoAbstinenser,
    opioidoppf.NEED_INFO_SIDE_EFFECTS AS InfoBivirkninger,
    opioidoppf.NEED_INFO_FREQUENT_FOLLOW_UP AS TettOppfolging,
    patient.REGISTERED_DATE AS DatoinhOppl,
    patient.BIRTH_DATE AS Fdato,
    patient.GENDER AS Kjonn,
    patient.PVK_RESERVATION_STATUS AS Reservasjonsstatus,
    patient.CONSENT AS SkrSamtykke,
    patient.CONSENT_DATE AS DatoinhSamt,
    patient.NOCONSENT_REASON AS ManglSamt,
    patient.CONSENT_WITHDRAW_DATE AS DatoTrektSamt,
    patient.DECEASED AS Avdod,
    patient.DECEASED_DATE AS Ddato,
    patient.ADDR_TYPE AS Adrtype,
    pateval.REGISTERED_DATE AS DatoUfylEval,
    pateval.COMPLETE AS PasdelUtfylEval,
    pateval.INCOMPLETE_REASON AS AarsakmanglUfylEval,
    pateval.PAINTEAM_CONTACT AS HuskerSmerteteam,
    pateval.PAINCHANGE AS EvalSpm1,
    pateval.GENERALCHANGE AS EvalSpm2,
    pateval.SATISFACTION AS EvalSpm3,
    pateval.PAINTEAM_COUMMINICATION AS EvalSpm4,
    pateval.PAINTEAM_TRUST AS EvalSpm5,
    pateval.PAINTEAM_INFO AS EvalSpm6,
    pateval.PAINTEAM_TREATMENT_PERSONALIZED AS EvalSpm7,
    pateval.PAINTEAM_PERSONAL_INVOLVEMENT AS EvalSpm8,
    pateval.PAINTEAM_ORGANIZATION AS EvalSpm9,
    pateval.PAINTEAM_TREATMENT_OVERALL AS EvalSpm10,
    pateval.PAINTEAM_MISTREAT AS EvalSpm11,
    patreg.REGISTERED_DATE AS DatoUfylPR,
    patreg.COMPLETE AS PasDelUfylPR,
    patreg.INCOMPLETE_REASON AS GrManglUtfylPR,
    patreg.TROUBLES_SLEEPING AS PRSpm1,
    patreg.SLEEPLESSNESS AS PRSpm2,
    patreg.CONTACT AS PRSpm3,
    patreg.CONTACT_MISS AS PRSpm3Mangler,
    patreg.HISTORY AS PRSpm4,
    patreg.PAINFOREVER AS PRSpm5,
    patreg.PAINNOTTOSTAND AS PRSpm6
    FROM
    mce mce INNER JOIN patient patient ON mce.PATIENT_ID = patient.ID
    INNER  JOIN emp11 emp11 ON mce.MCEID = emp11.MCEID
    INNER JOIN mcelist mcelist ON mce.MCEID = mcelist.MCEID
    LEFT OUTER  JOIN emp12 emp12 ON mce.MCEID = emp12.MCEID  AND emp12.FORMORDER = 1
    LEFT OUTER  JOIN emp12 emp21 ON mce.MCEID = emp21.MCEID  AND emp21.FORMORDER = 2
    LEFT OUTER  JOIN emp22 emp22 ON mce.MCEID = emp22.MCEID
    LEFT OUTER  JOIN hads hads ON mce.MCEID = hads.MCEID
    LEFT OUTER  JOIN opioidoppf opioidoppf ON mce.MCEID = opioidoppf.MCEID
    LEFT OUTER  JOIN pateval pateval ON mce.MCEID = pateval.MCEID
    LEFT OUTER  JOIN patreg patreg ON mce.MCEID = patreg.MCEID ",
                   userInput,
                   " AND mcelist.INCLUDED_RAPPORTEKET = 1 AND mcelist.MCE_COMPLETE = 1 AND mcelist.INCLUDED = 1
    AND
    CAST(
    (SELECT CASE mce.MCETYPE
     WHEN 1 THEN LEAST(mce.STATUS, emp11.STATUS)
     WHEN 2 THEN LEAST(
       mce.STATUS, emp11.STATUS, emp12.STATUS,
       emp21.STATUS, emp22.STATUS
     )
     WHEN 3 THEN LEAST(
       mce.STATUS, emp11.STATUS, emp12.STATUS,
       emp21.STATUS, emp22.STATUS
     )
     ELSE -1 END) AS SIGNED
     ) = 1
    AND CAST(
    (SELECT CASE
     WHEN MCETYPE != 1 THEN LEAST(
       IFNULL(hads.STATUS, 1),
       IFNULL(pateval.STATUS, 1),
       IFNULL(patreg.STATUS, 1),
       IFNULL(opioidoppf.STATUS, 1)
     )
     ELSE 1 END) AS SIGNED
    ) = 1")
  }

  if(tableName == "smertediagnosernum") {
    query = paste0("SELECT
                   epd.MCEID AS ForlopsID,
                   mce.PATIENT_ID AS PasientID,
                   mce.CENTREID AS AvdResh,
                   epd.ID AS SmerteDiagID,
                   epd.PAINCAT AS SmerteKat,
                   epd.PAINDIAG_CATEGORY AS DiagKat,
                   epd.PAINDIAG_SUBCATEGORY AS DiagSubKat,
                   epd.DIAGCODE AS ICD10Kode,
                   epd.DIAGDESCRIPTION AS ICD10Tekst,
                   epd.DIAG_VERSION AS ICD10Versjon,
                   epd.PAINMAINDIAG,
                   epd.CREATEDBY AS OpprettetAv
                   FROM mce mce
                   INNER JOIN emp11_pain_diagnosis epd ON COALESCE(NULLIF(mce.PARENT_ID, 'NA'), mce.MCEID) = epd.MCEID ",
                   userInput)
  }

  if(tableName == "smertediagnoser") {

    getListTextFunction(registryName)

    query = paste0("SELECT
    epd.MCEID AS ForlopsID,
    mce.PATIENT_ID AS PasientID,
    mce.CENTREID AS AvdResh,
    epd.ID AS SmerteDiagID,
    getListText('EMP11_PAINCAT',PAINCAT) AS SmerteKat,
    CASE PAINCAT
    WHEN 1 THEN getListText('EMP11_PAINDIAG_ACUTE_CATEGORY', PAINDIAG_CATEGORY)
    WHEN 2 THEN getListText('EMP11_PAINDIAG_CATEGORY', PAINDIAG_CATEGORY)
    WHEN 3 THEN	getListText('EMP11_PAINDIAG_CATEGORY', PAINDIAG_CATEGORY)
    WHEN 4 THEN 'Ikke aktuelt'
    ELSE 'Ukjent kategori'
    END AS DiagKat,
    scd.DESCRIPTION AS DiagSubKat,
    epd.DIAGCODE AS ICD10Kode,
    epd.DIAGDESCRIPTION AS ICD10Tekst,
    epd.DIAG_VERSION AS ICD10Versjon,
    getListText('EMP11_PAINMAINDIAG',PAINMAINDIAG) AS HovedDiag,
    epd.CREATEDBY AS OpprettetAv
    from
    emp11_pain_diagnosis epd LEFT OUTER JOIN subcatdescription scd ON epd.PAINDIAG_SUBCATEGORY = scd.SUBCAT
    AND epd.PAINDIAG_CATEGORY = scd.DIAGCAT
    INNER JOIN mce mce ON COALESCE(NULLIF(mce.PARENT_ID, 'NA'), mce.MCEID) = epd.MCEID ",
                   userInput)
  }

  return(query)
}
