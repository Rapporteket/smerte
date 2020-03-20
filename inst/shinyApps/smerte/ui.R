library(magrittr)
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(rapbase)

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
    )

  ) # navbarPage
) # tagList
