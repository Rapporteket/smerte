library(magrittr)
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(rapbase)
library(lubridate)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Smerteregisteret"

ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",

    tabPanel("Veiledning",
      useShinyalert(),
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"),
                            addUserInfo = TRUE),
        tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
    ),
    shiny::navbarMenu(
      "Rapporter",
      shiny::tabPanel(
        "Tilsyn",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("years"),
            shiny::radioButtons('formatTilsyn',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportTilsyn', 'Last ned')
          ),
          shiny::mainPanel(
            shiny::htmlOutput("tilsynsrapport", inline = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Dekningsgrad",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput('dateRangeDekningsgrad',
                                  label = "Velg periode:", start = "2017-01-01",
                                  end = Sys.Date(), separator = "-"),
            shiny::radioButtons('formatDekningsgrad',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportDekningsgrad', 'Last ned')
          ),
          shiny::mainPanel(
            htmlOutput("dekningsgrad", inline = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Indikatorer",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("indYears"),
            shiny::radioButtons('formatIndikator',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportIndikator', 'Last ned')
          ),
          shiny::mainPanel(
            shiny::htmlOutput("indikatorrapport", inline = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Eprom",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput('dateRangeEprom',
                                  label = "Velg periode:", start = "2017-01-01",
                                  end = Sys.Date(), separator = "-"),
            shiny::radioButtons('formatEprom',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportEprom', 'Last ned')
          ),
          shiny::mainPanel(
            shiny::htmlOutput("eprom", inline = TRUE) #%>%
            # withSpinner(color = "#18bc9c",color.background = "#ffffff",
            #             type = 2)
          )
        )
      ),
      shiny::tabPanel(
        "Spinalkateter",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput('dateRangeSpinalkateter',
                                  label = "Velg periode:", start = "2017-01-01",
                                  end = Sys.Date(), separator = "-"),
            shiny::radioButtons('formatSpinalkateter',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportSpinalkateter', 'Last ned')
          ),
          shiny::mainPanel(
            shiny::htmlOutput("spinalkateter", inline = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Smertekategori",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput('dateRangeSmertekategori',
                                  label = "Velg periode:", start = "2017-01-01",
                                  end = Sys.Date(), separator = "-"),
            shiny::radioButtons('formatSmertekategori',
                                'Format for nedlasting',
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton('downloadReportSmertekategori', 'Last ned')
          ),
          shiny::mainPanel(
            shiny::htmlOutput("smertekategori", inline = TRUE)
          )
        )
      )
    ),
    shiny::tabPanel(
      "Abonnement",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
                            shiny::uiOutput("subscriptionRepList"),
          selectInput("subscriptionFreq", "Frekvens:",
                      list(Årlig="Årlig-year",
                            Kvartalsvis="Kvartalsvis-quarter",
                            Månedlig="Månedlig-month",
                            Ukentlig="Ukentlig-week",
                            Daglig="Daglig-DSTday"),
                      selected = "Månedlig-month"),
          selectInput("subscriptionFileFormat", "Format:",
                      c("html", "pdf")),
          actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    ),

    shiny::tabPanel(
      "Abonnement NY!",
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

    shiny::navbarMenu("Verktøy",
      tabPanel(
        "Datadump",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            uiOutput("dumpTabControl"),
            dateRangeInput("dumpDateRange", "Velg periode:",
                           start = lubridate::ymd(Sys.Date())- years(1),
                           end = Sys.Date(), separator = "-",
                           weekstart = 1),
            radioButtons("dumpFormat", "Velg filformat:",
                         choices = list(
                           csv = "csv",
                           `csv2 (nordisk format)` = "csv2",
                           `xlsx-csv` = "xlsx-csv",
                           `xlsx-csv2 (nordisk format)` = "xlsx-csv2")
                         ),
            downloadButton("dumpDownload", "Hent!")
          ),
          mainPanel(
            htmlOutput("dumpDataInfo")
          )
        )
      ),
      shiny::tabPanel(
        "Metadata",
        shiny::sidebarLayout(
          shiny::sidebarPanel(uiOutput("metaControl")),
          shiny::mainPanel(htmlOutput("metaData"))
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
