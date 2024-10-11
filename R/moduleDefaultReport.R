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
#' @aliases defaultReportInput defaultReportUI defaultReportServer
#' defaultReportApp defaultReportServerReactive
NULL

#' @rdname defaultReport
#' @export
defaultReportInput <- function(
  id,
  startDate = lubridate::today() - lubridate::years(1),
  endDate = lubridate::today() - lubridate::weeks(1),
  min = "1980-01-01",
  max = "2100-01-01") {

  shiny::tagList(
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
    shiny::downloadButton(shiny::NS(id, "downloadReport"), "Last ned!")
  )

}


#' @rdname defaultReport
#' @export
defaultReportUI <- function(id) {
  shiny::tagList(
    shiny::htmlOutput(shiny::NS(id, "report"), inline = TRUE)
  )
}


#' @rdname defaultReport
#' @export
defaultReportServer <- function(id, reportFileName, reportParams) {
  shiny::moduleServer(id, function(input, output, session) {

    output$report <- shiny::renderUI({
      reportParams$startDate <- input$dateRange[1]
      reportParams$endDate <- input$dateRange[2]
      rapbase::renderRmd(
        sourceFile = system.file(reportFileName, package = "smerte"),
        outputType = "html_fragment",
        params = reportParams
      )
    })

    output$downloadReport <- shiny::downloadHandler(
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
        fn <- rapbase::renderRmd(
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
defaultReportServerReactive <- function(id, reportFileName, reportParamsReactive) {
  shiny::moduleServer(id, function(input, output, session) {

    output$report <- shiny::renderUI({
      req(input$dateRange)
      params <- reportParamsReactive()
      params$startDate <- input$dateRange[1]
      params$endDate <- input$dateRange[2]

      tryCatch({
        reportPath <- system.file(reportFileName, package = "smerte")
        if (reportPath == "") {
          stop("Report file not found in the 'smerte' package")
        }
        rapbase::renderRmd(
          sourceFile = reportPath,
          outputType = "html_fragment",
          params = params
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error rendering report:", e$message), type = "error")
        NULL
      })
    })

    output$downloadReport <- shiny::downloadHandler(
      filename = function() {
        paste0(
          sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(reportFileName)),
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".",
          input$format
        )
      },
      content = function(file) {
        params <- reportParamsReactive()
        params$startDate <- input$dateRange[1]
        params$endDate <- input$dateRange[2]
        params$tableFormat <- input$format

        tryCatch({
          reportPath <- system.file(reportFileName, package = "smerte")
          if (reportPath == "") {
            stop("Report file not found in the 'smerte' package")
          }
          fn <- rapbase::renderRmd(
            sourceFile = reportPath,
            outputType = input$format,
            params = params
          )
          file.rename(fn, file)
        }, error = function(e) {
          shiny::showNotification(paste("Error downloading report:", e$message), type = "error")
        })
      }
    )
  })
}


#' @rdname defaultReport
#' @export
defaultReportApp <- function() {

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        defaultReportInput("test")
      ),
      shiny::mainPanel(
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

  shiny::shinyApp(ui, server)
}
