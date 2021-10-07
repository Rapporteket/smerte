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
    tabPanel("Tilsynsrapport",
      sidebarLayout(
        sidebarPanel(
          uiOutput("years"),
          radioButtons('formatTilsyn',
                       'Format for nedlasting',
                       c('PDF', 'HTML'),
                       inline = FALSE),
          downloadButton('downloadReportTilsyn', 'Last ned')
        ),
        mainPanel(
          htmlOutput("tilsynsrapport", inline = TRUE) #%>%
            # withSpinner(color = "#18bc9c",color.background = "#ffffff",
            #             type = 2)
        )
      )
    ),
    tabPanel("Dekningsgrad",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput('dateRangeDekningsgrad',
                                label = "Velg periode:", start = "2017-01-01",
                                end = Sys.Date(), separator = "-"),
                 radioButtons('formatDekningsgrad',
                              'Format for nedlasting',
                              c('PDF', 'HTML'),
                              inline = FALSE),
                 downloadButton('downloadReportDekningsgrad', 'Last ned')
               ),
               mainPanel(
                 htmlOutput("dekningsgrad", inline = TRUE) #%>%
                 # withSpinner(color = "#18bc9c",color.background = "#ffffff",
                 #             type = 2)
               )
             )
    ),
    tabPanel("Indikatorrapport",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("indYears"),
                 radioButtons('formatIndikator',
                              'Format for nedlasting',
                              c('PDF', 'HTML'),
                              inline = FALSE),
                 downloadButton('downloadReportIndikator', 'Last ned')
               ),
               mainPanel(
                 htmlOutput("indikatorrapport", inline = TRUE) #%>%
                 # withSpinner(color = "#18bc9c",color.background = "#ffffff",
                 #             type = 2)
               )
             )
    ),
    tabPanel("Eprom",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput('dateRangeEprom',
                                label = "Velg periode:", start = "2017-01-01",
                                end = Sys.Date(), separator = "-"),
                 radioButtons('formatEprom',
                              'Format for nedlasting',
                              c('PDF', 'HTML'),
                              inline = FALSE),
                 downloadButton('downloadReportEprom', 'Last ned')
               ),
               mainPanel(
                 htmlOutput("eprom", inline = TRUE) #%>%
                 # withSpinner(color = "#18bc9c",color.background = "#ffffff",
                 #             type = 2)
               )
             )
    ),
    tabPanel("Spinalkateter",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput('dateRangeSpinalkateter',
                                label = "Velg periode:", start = "2017-01-01",
                                end = Sys.Date(), separator = "-"),
                 radioButtons('formatSpinalkateter',
                              'Format for nedlasting',
                              c('PDF', 'HTML'),
                              inline = FALSE),
                 downloadButton('downloadReportSpinalkateter', 'Last ned')
               ),
               mainPanel(
                 htmlOutput("spinalkateter", inline = TRUE) #%>%
                 # withSpinner(color = "#18bc9c",color.background = "#ffffff",
                 #             type = 2)
               )
             )
    ),
    tabPanel("Smertekategori",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput('dateRangeSmertekategori',
                                label = "Velg periode:", start = "2017-01-01",
                                end = Sys.Date(), separator = "-"),
                 radioButtons('formatSmertekategori',
                              'Format for nedlasting',
                              c('PDF', 'HTML'),
                              inline = FALSE),
                 downloadButton('downloadReportSmertekategori', 'Last ned')
               ),
               mainPanel(
                 htmlOutput("smertekategori", inline = TRUE) #%>%
                 # withSpinner(color = "#18bc9c",color.background = "#ffffff",
                 #             type = 2)
               )
             )
    ),
    tabPanel("Abonnement"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("subscriptionRepList"),
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
    tabPanel("Metadata"
      ,
      sidebarLayout(
        sidebarPanel(uiOutput("metaControl")),
        mainPanel(htmlOutput("metaData"))
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
        "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::exportUCInput("smerteExport")
          ),
          shiny::mainPanel(
            rapbase::exportGuideUI("smerteExportGuide")
          )
        )
      )
    )
  ) # navbarPage
) # tagList
