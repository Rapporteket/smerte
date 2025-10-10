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
        "Dekningsgrad før reservasjon",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("dekningsgrad",
                                       startDate = "2022-01-01",
                                       endDate = "2022-11-30",
                                       max = "2022-11-30")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("dekningsgrad")
          )
        )
      ),
      shiny::tabPanel(
        "Dekningsgrad etter reservasjon",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("dekningsgradReserv",
                                       startDate = "2022-12-01",
                                       min = "2022-12-01")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("dekningsgradReserv")
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
      shiny::tabPanel(
        "Variabeloversikt",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::variabeloversiktInput("variabeloversikt")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("variabeloversikt")
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
      ),
      shiny::tabPanel(
        "Epidural (barn)",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("lokalepi")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("lokalepi")
          )
        )
      ),
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
      shiny::tabPanel(
        "Oppfølging ved smerteklinikk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            smerte::defaultReportInput("oppfolg")
          ),
          shiny::mainPanel(
            smerte::defaultReportUI("oppfolg")
          )
        )
      )
    ),
    shiny::tabPanel(
      "Abonnement lokal",
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
      "Abonnement nasjonal",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportFormatInput("smerteSubscriptionNational"),
          rapbase::autoReportInput("smerteSubscriptionNational")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("smerteSubscriptionNational")
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
        "Utsendelser nasjonal",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportFormatInput("smerteDispatchmentNasjonal"),
            rapbase::autoReportOrgInput("smerteDispatchmentNasjonal"),
            shiny::HTML(
              "NB Dobbeltsjekk at rapporten er gitt riktig datakilde!<br/><br/>"
              ),
            rapbase::autoReportInput("smerteDispatchmentNasjonal")
            ),
          shiny::mainPanel(
            rapbase::autoReportUI("smerteDispatchmentNasjonal")
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
