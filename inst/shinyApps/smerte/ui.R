library(shiny)
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
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"))
      )
    ),
    tabPanel("Tilsynsrapport",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(inputId = "period", label = "Periode:",
                         start = "2017-01-01", end = NULL,
                         format = "dd-mm-yyyy", language = "nb",
                         weekstart = 1),
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
    tabPanel("Figur og tabell"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(inputId = "var",
                      label = "Variabel:",
                      c("mpg", "disp", "hp", "drat", "wt", "qsec")),
          sliderInput(inputId = "bins",
                      label = "Antall grupper:",
                      min = 1,
                      max = 12,
                      value = 5)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Figur", plotOutput("distPlot")),
            tabPanel("Tabell", tableOutput("distTable"))
          )
        )
      )
    ),
    # tabPanel("Sammendrag",
    # sidebarLayout(
    #   sidebarPanel(width = 3,
    #                selectInput(inputId = "var",
    #                            label = "Variabel:",
    #                            c("mpg", "disp", "hp", "drat", "wt", "qsec")),
    #                sliderInput(inputId = "bins",
    #                            label = "Antall grupper:",
    #                            min = 1,
    #                            max = 12,
    #                            value = 5)
    #   ),
    #            mainPanel(
    #              tabsetPanel(
    #                tabPanel("Sammendrag", tableOutput("distSummary")),
    #              )
    #            )
    #   )
    # ),
    tabPanel("Samlerapport"
        ,
        tabPanel("Fordeling av mpg",
          sidebarLayout(
            sidebarPanel(width = 3,
              selectInput(inputId = "varS",
                          label = "Variabel:",
                          c("mpg", "disp", "hp", "drat", "wt", "qsec")),
              sliderInput(inputId = "binsS",
                          label = "Antall grupper:",
                          min = 1,
                          max = 12,
                          value = 5),
              downloadButton("downloadSamlerapport", "Last ned!")
            ),
            mainPanel(
              uiOutput("samlerapport")
            )
          )
        )
      ),
    tabPanel("Abonnement"
      # ,
      # sidebarLayout(
      #   sidebarPanel(width = 3,
      #     selectInput("subscriptionRep", "Rapport:", c("Samlerapport1", "Samlerapport2")),
      #     selectInput("subscriptionFreq", "Frekvens:",
      #                 list(Årlig="year", Kvartalsvis="quarter", Månedlig="month", Ukentlig="week", Daglig="DSTday"),
      #                 selected = "month"),
      #     actionButton("subscribe", "Bestill!")
      #   ),
      #   mainPanel(
      #     uiOutput("subscriptionContent")
      #   )
      # )
    )

  ) # navbarPage
) # tagList