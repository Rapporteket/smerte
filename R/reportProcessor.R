#' Common report processor for SmerteReg
#'
#' Makes reports for SmerteReg typically used for auto reports such as
#' subscriptions, dispatchments and bulletins. As such, please be warned that
#' any changes to this function might render existing auto reports nonfunctional
#' as they are based on static calls based on any previous version of this
#' function. Changes should therefore be thoroughly tested against existing auto
#' reports. Altering the names of the arguments will likely be a breaking
#' change. Adding new arguments should be safe as long as they are provided a
#' default value.
#'
#' @param report Character string identifying the report to be processed by this
#' function.
#' @param outputType Character string with output format. Must be one of
#' \code{c("html", "pdf")}. Defaults to "pdf".
#' @param title Character string giving the report title. Empty string by
#' default.
#' @param author Character string providing the report author. Default value is
#' "unknown author".
#' @param orgName Character string with the name of the organization/hospital.
#' Default is "unknown organization".
#' @param orgId Integer (?) with the id of the organization/hospital. Default is
#' 999999.
#' @param registryName Character string with registry name. Default is
#' "ablanor".
#' @param userFullName Character string giving the person name, normally the
#' user requesting the report. Default is "unknown person name".
#' @param userRole Character string giving a user role, normally the one of the
#' user requesting the report. Default is "unknown role".
#' @param year Integer year most likely for selecting data for the report.
#' Defaults to the current year.
#' @param startDate Character string with the format "YYYY-MM-DD" providing the
#' start of a period. Defaults to January 1st of of the year given in
#' \code{year}.
#' @param endDate Character string with the format "YYYY-MM-DD" providing the
#' end of a period. Defaults to December 31st of of the year given in
#' \code{year}.
#'
#' @return A character string with a path to where the produced file is located.
#' @export
#'
#' @examples
#' ## Make the start page for SmerteReg
#' reportFilePath <- reportProcessor(report = "veiledning",
#'                                   title = "Example report")

reportProcessor <- function(report,
                            outputType = "pdf",
                            title = "",
                            author = "unknown author",
                            orgName = "unknown organization",
                            orgId = 999999,
                            registryName = "unknown registry",
                            userFullName = "unknown person name",
                            userRole = "unknown role",
                            year = format(Sys.Date(), "%Y"),
                            startDate = paste(year, "01", "01", sep = "-"),
                            endDate = paste(year, "12", "31", sep = "-")) {

  stopifnot(report %in% c("veiledning", "tilsyn", "spinalkateter"))

  stopifnot(outputType %in% c("html", "pdf"))

  filePath <- NULL

  if (title == "") {
    warning("No title given! Reports should have a title...")
  }

  if (report == "veiledning") {
    filePath <- rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "smerte"),
      outputType = outputType,
      params = list(
        title = title,
        author = author,
        hospitalName = orgName,
        tableFormat = outputType,
        reshId = orgId
      )
    )
  }

  if (report == "tilsyn") {
    filePath <- rapbase::renderRmd(
      system.file("LokalTilsynsrapportMaaned.Rmd", package = "smerte"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = outputType,
        reshId = orgId,
        registryName = registryName,
        userRole = userRole,
        year = year
      )
    )
  }

  if (report == "spinalkateter") {
    filePath <- rapbase::renderRmd(
      system.file("LokalSpinalkateter.Rmd", package = "smerte"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = outputType,
        reshId = orgId,
        registryName = registryName,
        userRole = userRole,
        year = year,
        startDate = startDate,
        endDate = endDate
      )
    )
  }

  filePath
}
