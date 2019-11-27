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

    tabPanel("Veiledning",
      useShinyalert(),
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"),
                            addUserInfo = TRUE)
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
          htmlOutput("tilsynsrapport", inline = TRUE) %>%
            withSpinner(color = "#18bc9c",color.background = "#ffffff",
                        type = 2)
        )
      )
    ),

    tabPanel("Abonnement"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("subscriptionRep", "Rapport:",
                      c("Lokalt tilsyn per måned 2016",
                        "Lokalt tilsyn per måned 2017")),
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
    )

  ) # navbarPage
) # tagList
