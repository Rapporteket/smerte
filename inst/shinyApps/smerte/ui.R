regTitle = "Smerteregisteret"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = rapbase::regTitle(regTitle),
    windowTitle = regTitle,
    theme = rapbase::rapTheme(),
    id = "tabs",

    shiny::tabPanel(
      title = "Veiledning",
      value = "tab_veiledning",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      shiny::mainPanel(width = 12,
        shiny::htmlOutput("veiledning", inline = TRUE)
      )
    ),
    shiny::navbarMenu(
      title = "Rapporter",
      shiny::tabPanel(
        title = "Indikatorer",
        value = "tab_indikatorer",
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
        title = "Opioidreduksjon",
        value = "tab_opioidreduksjon",
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
        title = "Eprom",
        value = "tab_eprom",
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
      #   title = "Epidural - barn",
      #   value = "tab_epidural_barn",
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
        title = "Tid til død etter utskrivelse",
        value = "tab_tid_til_dod",
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
      title = "Datadump",
      value = "tab_datadump",
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
