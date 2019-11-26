library(shiny)
library(shinyalert)
library(rapbase)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Smerteregisteret"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))

ui <- tagList(
  navbarPage(
    # title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
    #             regTitle),
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
                       c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
                       inline = FALSE),
          downloadButton('downloadReportTilsyn', 'Last ned')
        ),
        mainPanel(
          htmlOutput("tilsynsrapport", inline = TRUE)
        )
      )
    ),

    tabPanel("Abonnement"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("subscriptionRep", "Rapport:", c("Lokalt tilsyn",
                                                       "Nasjonalt tilsyn")),
          selectInput("subscriptionFreq", "Frekvens:",
                      list(Årlig="Årlig-year",
                            Kvartalsvis="Kvartalsvis-quarter",
                            Månedlig="Månedlig-month",
                            Ukentlig="Ukentlig-week",
                            Daglig="Daglig-DSTday"),
                      selected = "Månedlig-month"),
          actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    )

  ) # navbarPage
) # tagList
