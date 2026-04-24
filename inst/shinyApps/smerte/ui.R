regTitle = "Smerteregisteret"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = rapbase::title(regTitle),
    windowTitle = regTitle,
    theme = rapbase::theme(),
    id = "tabs",

    shiny::tabPanel(
      "Veiledning",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      shiny::mainPanel(width = 12,
        shiny::htmlOutput("veiledning", inline = TRUE)
      )
    ),
    shiny::navbarMenu(
      "Rapporter",
      shiny::tabPanel(
        "Indikatorer",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("indikator")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("indikator")
          )
        )
      ),
      shiny::tabPanel(
        "Opioidreduksjon",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("opioid")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("opioid")
          )
        )
      ),
      shiny::tabPanel(
        "Eprom",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("eprom")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("eprom")
          )
        )
      ),
      # shiny::tabPanel(
      #   "Epidural - barn",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       smerte::defaultReportInput("lokalepi")
      #       ),
      #       shiny::mainPanel(
      #         smerte::defaultReportUI("lokalepi")
      #       )
      #     )
      #   ),
      shiny::tabPanel(
        "Tid til død etter utskrivelse",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("timetodeath")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("timetodeath")
          )
        )
      ),
     ),
    shiny::tabPanel(
      "Datadump",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::uiOutput("dumpTabControl"),
          shiny::dateRangeInput(
            "dumpDateRange",
            "Velg periode:",
            start = lubridate::ymd(Sys.Date()) - lubridate::years(1),
            end = Sys.Date(),
            separator = "-",
            weekstart = 1
          ),
          shiny::radioButtons(
            "dumpFormat",
            "Velg filformat:",
            choices = list(
              csv = "csv",
              `csv2 (nordisk format)` = "csv2",
              `xlsx-csv` = "xlsx-csv",
              `xlsx-csv2 (nordisk format)` = "xlsx-csv2"
            )
          ),
          shiny::downloadButton("dumpDownload", "Hent!")
        ),
        shiny::mainPanel(
          shiny::htmlOutput("dumpDataInfo")
        )
      )
    ),
  ) # navbarPage
) # tagList
