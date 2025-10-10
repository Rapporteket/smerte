#' Module for VariabeloversiktRapport
#'
#' @param id Character string with module id (namespace).
#' @param startDate Character string of the form YYYY-MM-DD or Date object
#' providing start date. Default is one year ago.
#' @param endDate Character string of the form YYYY-MM-DD or Date object
#' providing end date. Default is today minus one week.
#' @param min Minimum value for startDate and endDate, Default is '1980-01-01'.
#' @param max Maximum value for startDate and endDate, Default is '2100-01-01'.
#'
#' @return Shiny object
#' @name variabelOversiktRapport
#' @aliases variabeloversiktInput
NULL

#' @rdname variabelOversiktRapport
#' @export
variabeloversiktInput <- function(
    id,
    startDate = lubridate::today() - lubridate::years(1),
    endDate = lubridate::today() - lubridate::weeks(1),
    min = "1980-01-01",
    max = "2100-01-01") {

  shiny::tagList(
    shiny::selectInput("avdValg", "Velg Avdeling:", choices = NULL),
    shiny::dateRangeInput(shiny::NS(id, "dateRange"),
                          label = "Velg periode:",
                          start = startDate,
                          end = endDate,
                          min = min,
                          max = max,
                          separator = "-"),
    shiny::radioButtons(shiny::NS(id, "format"),
                        "Format for nedlasting",
                        list(PDF = "pdf", HTML = "html"),
                        inline = FALSE),
    shiny::downloadButton(shiny::NS(id, "downloadReport"), "Last ned!")#,
    #   actionButton("generate", "Generer Rapport", class = "btn-primary")
  )

}
