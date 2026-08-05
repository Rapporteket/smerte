#' Shiny modules for Smertereg at Rapportket
#'
#' @param id Character string with module id (namespace).
#' @param startDate Character string of the form YYYY-MM-DD or Date object
#' providing start date. Default is one year ago.
#' @param endDate Character string of the form YYYY-MM-DD or Date object
#' providing end date. Default is today minus one week.
#' @param reportFileName Character string providing basename of the file
#' representing the report template. Such templates must be placed directly
#' under the inst directory of the package.
#' @param reportParams A named list of parameters that will be used by the
#' report template.
#'
#' @return Shiny object
#' @name defaultReport
NULL

#' @rdname defaultReport
#' @export
defaultReportInput <- function(
  id,
  startDate = today() - years(1),
  endDate = today() - weeks(1),
  min = "1980-01-01",
  max = "2100-01-01") {

  tagList(
    dateRangeInput(NS(id, "dateRange"),
                          label = "Velg periode:",
                          start = startDate,
                          end = endDate,
                          min = min,
                          max = max,
                          separator = "-"),
    radioButtons(NS(id, "format"),
                        "Format for nedlasting",
                        list(PDF = "pdf", HTML = "html"),
                        inline = FALSE),
    downloadButton(NS(id, "downloadReport"), "Last ned!")
  )

}


#' @rdname defaultReport
#' @export
defaultReportUI <- function(id) {
  tagList(
    htmlOutput(NS(id, "report"), inline = TRUE)
  )
}


#' @rdname defaultReport
#' @export
defaultReportServer <- function(id, reportFileName, reportParams) {
  moduleServer(id, function(input, output, session) {

    output$report <- renderUI({
      reportParams$startDate <- input$dateRange[1]
      reportParams$endDate <- input$dateRange[2]
      renderRmd(
        sourceFile = system.file(reportFileName, package = "smerte"),
        outputType = "html_fragment",
        params = reportParams
      )
    })

    output$downloadReport <- downloadHandler(
      filename = function() {
        basename(
          tempfile(
            pattern = sub(pattern = "(.*?)\\..*$", replacement = "\\1",
                          basename(reportFileName)),
            fileext = paste0(".", input$format)
          )
        )
      },
      content = function(file) {
        reportParams$startDate <- input$dateRange[1]
        reportParams$endDate <- input$dateRange[2]
        reportParams$tableFormat <- input$format
        fn <- renderRmd(
          sourceFile = system.file(reportFileName, package = "smerte"),
          outputType = input$format,
          params = reportParams
        )
        file.rename(fn, file)
      }
    )
  })
}

#' @rdname defaultReport
#' @export
defaultReportServer2 <- function(id, reportFileName, reportParams) {
  moduleServer(id, function(input, output, session) {

    output$report <- renderUI({
      reportParams_list <- reportParams()
      reportParams_list$startDate <- input$dateRange[1]
      reportParams_list$endDate <- input$dateRange[2]
      renderRmd(
        sourceFile = system.file(reportFileName(), package = "smerte"),
        outputType = "html_fragment",
        params = reportParams_list
      )
    })

    output$downloadReport <- downloadHandler(
      filename = function() {
        basename(
          tempfile(
            pattern = sub(pattern = "(.*?)\\..*$", replacement = "\\1",
                          basename(reportFileName())),
            fileext = paste0(".", input$format)
          )
        )
      },
      content = function(file) {
        reportParams_list <- reportParams()
        reportParams_list$startDate <- input$dateRange[1]
        reportParams_list$endDate <- input$dateRange[2]
        reportParams_list$tableFormat <- input$format
        fn <- renderRmd(
          sourceFile = system.file(reportFileName(), package = "smerte"),
          outputType = input$format,
          params = reportParams_list
        )
        file.rename(fn, file)
      }
    )
  })
}


#' @rdname defaultReport
#' @export
defaultReportApp <- function() {

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        defaultReportInput("test")
      ),
      mainPanel(
        defaultReportUI("test")
      )
    )
  )

  server <- function(input, output, session) {

    params = list(hospitalName = "Testsykehus",
                  reshId = 100082,
                  registryName = "Testregister",
                  userRole = "LU",
                  userFullName = "Tore Tester",
                  shinySession = session)

    defaultReportServer("test", "sampleReport.Rmd", params)
  }

  shinyApp(ui, server)
}
