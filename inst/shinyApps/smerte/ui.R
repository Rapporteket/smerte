regTitle = "Smerteregisteret"

ui <- tagList(
  navbarPage(
    title = regTitle(regTitle),
    windowTitle = regTitle,
    theme = rapTheme(),
    id = "tabs",

    tabPanel(
      title = "Veiledning",
      value = "tab_veiledning",
      navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE)
      )
    ),
    navbarMenu(
      title = "Rapporter",
      tabPanel(
        title = "Indikatorer",
        value = "tab_indikatorer",
        sidebarLayout(
          sidebarPanel(
            defaultReportInput("indikator")
          ),
          mainPanel(
            defaultReportUI("indikator")
          )
        )
      ),
      tabPanel(
        title = "Opioidreduksjon",
        value = "tab_opioidreduksjon",
        sidebarLayout(
          sidebarPanel(
            defaultReportInput("opioid")
          ),
          mainPanel(
            defaultReportUI("opioid")
          )
        )
      ),
      tabPanel(
        title = "Eprom",
        value = "tab_eprom",
        sidebarLayout(
          sidebarPanel(
            defaultReportInput("eprom")
          ),
          mainPanel(
            defaultReportUI("eprom")
          )
        )
      ),
      # tabPanel(
      #   title = "Epidural - barn",
      #   value = "tab_epidural_barn",
      #   sidebarLayout(
      #     sidebarPanel(
      #       defaultReportInput("lokalepi")
      #       ),
      #       mainPanel(
      #         defaultReportUI("lokalepi")
      #       )
      #     )
      #   ),
      tabPanel(
        title = "Tid til død etter utskrivelse",
        value = "tab_tid_til_dod",
        sidebarLayout(
          sidebarPanel(
            defaultReportInput("timetodeath")
          ),
          mainPanel(
            defaultReportUI("timetodeath")
          )
        )
      ),
     ),
    tabPanel(
      title = "Datadump",
      value = "tab_datadump",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          uiOutput("dumpTabControl"),
          dateRangeInput(
            "dumpDateRange",
            "Velg periode:",
            start = ymd(Sys.Date()) - years(1),
            end = Sys.Date(),
            separator = "-",
            weekstart = 1
          ),
          radioButtons(
            "dumpFormat",
            "Velg filformat:",
            choices = list(
              csv = "csv",
              `csv2 (nordisk format)` = "csv2",
              `xlsx-csv` = "xlsx-csv",
              `xlsx-csv2 (nordisk format)` = "xlsx-csv2"
            )
          ),
          downloadButton("dumpDownload", "Hent!")
        ),
        mainPanel(
          htmlOutput("dumpDataInfo")
        )
      )
    ),
  ) # navbarPage
) # tagList
