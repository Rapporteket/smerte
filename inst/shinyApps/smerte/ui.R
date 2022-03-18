shiny::addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Smerteregisteret"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = shiny::div(
      shiny::a(
        shiny::includeHTML(system.file('www/logo.svg', package='rapbase'))),
      regTitle
    ),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",

    shiny::tabPanel("Veiledning",
      shinyalert::useShinyalert(),
      shiny::mainPanel(width = 12,
        shiny::htmlOutput("veiledning", inline = TRUE),
        rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                     organization = uiOutput("appOrgName"),
                                     addUserInfo = TRUE),
        shiny::tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
    ),
    shiny::navbarMenu(
      "Rapporter",
      shiny::tabPanel(
        "Tilsyn",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("tilsyn")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("tilsyn")
          )
        )
      ),
      shiny::tabPanel(
        "Dekningsgrad",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("dekningsgrad")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("dekningsgrad")
          )
        )
      ),
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
      shiny::tabPanel(
        "Spinalkateter",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("spinalkateter")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("spinalkateter")
          )
        )
      ),
      shiny::tabPanel(
        "Smertekategori",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("smertekategori")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("smertekategori")
          )
        )
      )
    ),
    shiny::tabPanel(
      "Abonnement",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportFormatInput("smerteSubscription"),
          rapbase::autoReportInput("smerteSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("smerteSubscription")
        )
      )
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
    shiny::navbarMenu("Verktøy",
      shiny::tabPanel(
        "Metadata",
        shiny::sidebarLayout(
          shiny::sidebarPanel(uiOutput("metaControl")),
          shiny::mainPanel(htmlOutput("metaData"))
        )
      ),
      shiny::tabPanel(
        "Utsendelser",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportFormatInput("smerteDispatchment"),
            rapbase::autoReportOrgInput("smerteDispatchment"),
            shiny::HTML(
              "NB Dobbeltsjekk at rapporten er gitt riktig datakilde!<br/><br/>"
            ),
            rapbase::autoReportInput("smerteDispatchment")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("smerteDispatchment")
          )
        )
      ),
      shiny::tabPanel(
        "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::exportUCInput("smerteExport")
          ),
          shiny::mainPanel(
            rapbase::exportGuideUI("smerteExportGuide")
          )
        )
      ),
      shiny::tabPanel(
        "Bruksstatisitkk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::statsInput("smerteStats"),
            rapbase::statsGuideUI("smerteStats")
          ),
          shiny::mainPanel(
            rapbase::statsUI("smerteStats")
          )
        )
      )
    )
  ) # navbarPage
) # tagList
