server <- function(input, output, session) {

  # logShinyInputChanges(input)

  map_db_resh <-
    getConfig("rapbaseConfig.yml")$reg$smerte$databases |>
    unlist() |>
    matrix(nrow=2) |>
    t() |>
    as.data.frame() |>
    rename(orgname = V1, UnitId = V2)

  map_orgname <- fikse_sykehusnavn(map_db_resh %>% select(-orgname), "UnitId")

  user <- navbarWidgetServer2(
    "navbar-widget",
    orgName = "smerte",
    caller = "smerte",
    map_orgname = req(map_orgname)
  )

  appLogger(session, msg = "Starting smerte app")

  # Parameters that may change depending on the role and org of user
  ## setting values that do depend on a Rapporteket context
  if (isRapContext()) {
    registryName <- reactive(
      map_db_resh$orgname[map_db_resh$UnitId == user$org()])
    userFullName <- Sys.getenv("FALK_USER_FULLNAME")
    hospitalName <- reactive(getHospitalName(
      registryName(),
      user$org(),
      user$role())
    )
  }

# Nasjonal database -------------------------------------------------------
  observeEvent(user$org(), {

    #fjerne dynamiske tabs
    removeTab(inputId = "tabs", target = "tab_tilsyn")
    removeTab(inputId = "tabs", target = "tab_dg_for_res")
    removeTab(inputId = "tabs", target = "tab_dg_etter_res")
    removeTab(inputId = "tabs", target = "tab_spinalkateter")
    removeTab(inputId = "tabs", target = "tab_smertekategori")
    removeTab(inputId = "tabs", target = "tab_oppf_smerteklinikk")
    # removeTab(inputId = "tabs", target = "tab_epidural_barn")
    removeTab(inputId = "tabs", target = "tab_abb_lokal")
    removeTab(inputId = "tabs", target = "tab_abb_nasjonal")

    if (isNationalReg(req(user$org()))) {

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Abonnement nasjonal",
                         value = "tab_abb_nasjonal",
                         sidebarLayout(
                           sidebarPanel(
                             autoReportFormatInput("smerteSubscriptionNational"),
                             autoReportInput("smerteSubscriptionNational")
                           ),
                           mainPanel(
                             autoReportUI("smerteSubscriptionNational")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_datadump")
    } else {

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Tilsyn",
                         value = "tab_tilsyn",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("tilsyn")
                           ),
                           mainPanel(
                             defaultReportUI("tilsyn")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_indikatorer"
                       )

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Dekningsgrad før reservasjon",
                         value = "tab_dg_for_res",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("dekningsgrad")
                           ),
                           mainPanel(
                             defaultReportUI("dekningsgrad")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_indikatorer"
                       )

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Dekningsgrad etter reservasjon",
                         value = "tab_dg_etter_res",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("dekningsgradReserv")
                           ),
                           mainPanel(
                             defaultReportUI("dekningsgradReserv")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_indikatorer"
                       )

      insertTab(inputId = "tabs",
                       tab =  tabPanel(
                         title = "Spinalkateter",
                         value = "tab_spinalkateter",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("spinalkateter")
                           ),
                           mainPanel(
                             defaultReportUI("spinalkateter")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_tid_til_dod"
                       )

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Smertekategori",
                         value = "tab_smertekategori",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("smertekategori")
                           ),
                           mainPanel(
                             defaultReportUI("smertekategori")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_tid_til_dod"
                       )

      insertTab(inputId = "tabs",
                       tab =       tabPanel(
                         title = "Oppfølging ved smerteklinikk",
                         value = "tab_oppf_smerteklinikk",
                         sidebarLayout(
                           sidebarPanel(
                             defaultReportInput("oppfolg")
                           ),
                           mainPanel(
                             defaultReportUI("oppfolg")
                           )
                         )
                       ),
                       position = "after",
                       target = "tab_tid_til_dod"
                       )

      insertTab(inputId = "tabs",
                       tab = tabPanel(
                         title = "Abonnement lokal",
                         value = "tab_abb_lokal",
                         sidebarLayout(
                           sidebarPanel(
                             autoReportFormatInput("smerteSubscription"),
                             autoReportInput("smerteSubscription")
                           ),
                           mainPanel(
                             autoReportUI("smerteSubscription")
                           )
                         )
                       ),
                       position = "before",
                       target = "tab_datadump")
    }
  }
  )

# Tilgangsnivå -----------------------------------------------------------

  observeEvent(list(user$role(), user$org()), {
    req(user$role(), user$org())

    removeTab(inputId = "tabs", target = "Verktøy")
    removeTab("tabs", target = "tab_utsendelser")
    removeTab("tabs", target = "tab_utsendelser_nasjonal")

    if (req(user$role()) %in% "SC") {
      insertTab(
        inputId = "tabs",
        tab = navbarMenu(
          title = "Verktøy",
          tabPanel(
            title = "Metadata",
            value = "tab_metadata",
            sidebarLayout(
              sidebarPanel(uiOutput("metaControl")),
              mainPanel(htmlOutput("metaData"))
              )
            ),
          tabPanel(
            title = "Eksport",
            value = "tab_eksport",
            sidebarLayout(
              sidebarPanel(
                exportUCInput("smerteExport")
                ),
              mainPanel(
                exportGuideUI("smerteExportGuide")
                )
              )
            ),
          tabPanel(
            title = "Bruksstatisitkk",
            value = "tab_bruksstatistikk",
            sidebarLayout(
              sidebarPanel(
                statsInput("smerteStats"),
                statsGuideUI("smerteStats")
                ),
              mainPanel(
                statsUI("smerteStats")
                )
              )
            ),
        if(isNationalReg(user$org())) tabPanel(
            title = "Utsendelser nasjonal",
            value = "tab_utsendelser_nasjonal",
            sidebarLayout(
              sidebarPanel(
                autoReportFormatInput("smerteDispatchmentNasjonal"),
                autoReportOrgInput("smerteDispatchmentNasjonal"),
                HTML(
                  "NB Dobbeltsjekk at rapporten er gitt riktig datakilde!<br/><br/>"
                  ),
                autoReportInput("smerteDispatchmentNasjonal")
                ),
              mainPanel(
                autoReportUI("smerteDispatchmentNasjonal")
                )
              )
            ) else tabPanel(
             title = "Utsendelser",
             value = "tab_utsendelser",
             sidebarLayout(
               sidebarPanel(
                 autoReportFormatInput("smerteDispatchment"),
                 autoReportOrgInput("smerteDispatchment"),
                 HTML(
                   "NB Dobbeltsjekk at rapporten er gitt riktig datakilde!<br/><br/>"
                   ),
                 autoReportInput("smerteDispatchment")
                 ),
               mainPanel(
                 autoReportUI("smerteDispatchment")
                 )
               )
             )
          ),
        position = "after",
        target = "tab_datadump"
      )
    }
  }
  )

  contentDump <- function(file, type, userRole = "LU") {

    d <- getDataDump(registryName(),
                             tableName = input$dumpDataSet,
                             reshId = user$org(),
                             userRole = user$role(),
                             fromDate = input$dumpDateRange[1],
                             toDate = input$dumpDateRange[2],
                             session = session)

    if (userRole %in% c("SC", "LC")) {
      if (input$dumpDataSet %in% c("smertediagnoser", "smertediagnosernum", "smertediagnosernumnasjonal")) {

        d = d %>% fikse_sykehusnavn("AvdResh") %>%
          relocate(SykehusNavn,
                   .after = "AvdResh")
      }
    }

    if (type == "xlsx-csv") {
      write_excel_csv2(d, file)
    } else {
      write_csv2(d, file)
    }
  }


  # Veiledning
  output$veiledning <- renderUI({
    #htmlRenderRmd("veiledning.Rmd")
    renderRmd(
      system.file("veiledning.Rmd", package = "smerte"),
      outputType = "html_fragment"
    )
  })

  reportParams <- reactive(
    list(
      hospitalName = hospitalName(),
      reshId = user$org(),
      registryName = registryName(),
      userRole = user$role(),
      userFullName = userFullName,
      shinySession = session
    )
  )

  # # Tilsynsrapport
  defaultReportServer2(
    id = "tilsyn",
    reportFileName = reactiveVal("LokalTilsynsrapportMaaned.Rmd"),
    reportParams = reportParams
  )

  # Dekningsgrad gammel
  defaultReportServer2(
    id = "dekningsgrad",
    reportFileName = reactiveVal("LokalDekningsgradrapport.Rmd"),
    reportParams = reportParams)
  # Dekningsgrad ny
  defaultReportServer2(
    id = "dekningsgradReserv",
    reportFileName = reactiveVal("LokalDekningsgradrapportReservasjon.Rmd"),
    reportParams = reportParams)

  # Indikatorrapport
  reportTemplate <- reactiveVal()
  observeEvent(user$org(), {
    if (isNationalReg(user$org())) {
      reportTemplate("NasjonalIndikatorMaaned.Rmd")
    } else {
      reportTemplate("LokalIndikatorMaaned.Rmd")
    }
  }
  )

  defaultReportServer2(
    id = "indikator",
    reportFileName = reportTemplate,
    reportParams = reportParams)

  # Opiodreduksjon

  reportTemplate2 <- reactiveVal()
  observeEvent(user$org(), {
    if (isNationalReg(user$org())) {
      reportTemplate2("NasjonalOpioidReduksjon.Rmd")
    } else {
      reportTemplate2("LokalOpioidReduksjon.Rmd")
    }
  }
  )

  defaultReportServer2(
    id = "opioid",
    reportFileName = reportTemplate2,
    reportParams = reportParams)


  # eProm
  defaultReportServer2(
    id = "eprom",
    reportFileName = reactiveVal("lokalEprom.Rmd"),
    reportParams = reportParams)

  # Spinalkateter
  defaultReportServer2(
    id = "spinalkateter",
    reportFileName = reactiveVal("LokalSpinalkateter.Rmd"),
    reportParams = reportParams)

  # Smertekategori
  defaultReportServer2(
    id = "smertekategori",
    reportFileName = reactiveVal("LokalSmertekategori.Rmd"),
    reportParams = reportParams)

  # Tid til død
  defaultReportServer2(
    id = "timetodeath",
    reportFileName = reactiveVal("timetodeath.Rmd"),
    reportParams = reportParams)

  # Oppfølging ved smerteklinikk
  defaultReportServer2(
    id = "oppfolg",
    reportFileName = reactiveVal("LokalOppfolg.Rmd"),
    reportParams = reportParams)
  # Epidural hos barn
  defaultReportServer2(
    id = "lokalepi",
    reportFileName = reactiveVal("LokalEpidural.Rmd"),
    reportParams = reportParams)

  # Definisjon av rapporter for abonnement og utsendelser

  nationalReports <- list(
    `Kvalitetsindikatorer - alle enheter`= list(
      synopsis = paste("Kvalitetsindikatorer fra Smerteregisteret",
                       "(alle enheter)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title",
                     "author", "orgName", "orgId",
                     "registryName", "userFullName"),
      paramValues = c("nasjonalIndikator", "pdf", "Kvalitetsindikatorer",
                      "Smerteregisteret", "sykehus", 99999,
                      "smertedata", userFullName)
    )
  )
  localReports <- list(
    `Tilsyn - lokal enhet` = list(
      synopsis = paste("Smerteregisteret: månedlig oppsummering av tilsyn",
                       "siste år (lokal enhet)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "author",
                     "orgName", "orgId", "registryName", "userFullName",
                     "userRole"),
      paramValues = c("tilsyn", "pdf", "Tilsyn", "Smerteregisteret",
                      "sykehus", 99999, "smertedata", userFullName,
                      "LU")
    ),
    `Kvalitetsindikatorer - lokal enhet` = list(
      synopsis = paste("Kvalitetsindikatorer fra Smerteregisteret",
                       "(lokal enhet)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title",
                     "author", "orgName", "orgId",
                     "userFullName", "userRole", "registryName"),
      paramValues = c("indikator", "pdf", "Kvalitetsindikatorer",
                      "Smerteregisteret", "sykehus", 99999,
                      userFullName, "LU", "smertedata")
    ),
    `Spinalkateter - lokal enhet` = list(
      synopsis = paste("Smerteregisteret: bruk av spinalkateter inneværende",
                       "år (lokal enhet)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title",
                     "author", "orgId", "userFullName", "userRole",
                     "registryName", "orgName"),
      paramValues = c("spinalkateter", "pdf", "Spinalkateter",
                      "Smerteregisteret", 99999, userFullName, "LU",
                      "smertedata", "sykehus")
    )
  )

  ## set reactive parameters overriding those in the reports list
  subParamNames <- reactive(c("registryName", "orgName", "orgId",
                                     "userRole"))
  subParamValues <- reactive(c(registryName(), hospitalName(), user$org(),
                                      user$role()))

  # Abonnement

  autoReportServer(
    "smerteSubscription",
    registryName = "smerte",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = localReports,
    freq = "quarter",
    user = user
  )

  autoReportServer(
    "smerteSubscriptionNational",
    registryName = "smerte",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = nationalReports,
    freq = "quarter",
    user = user
  )

  # # Utsendelser
  format <- autoReportFormatServer("smerteDispatchment")
  format2 <- autoReportFormatServer("smerteDispatchmentNasjonal")

  orgs <- c(list(`Alle nasjonale data` = "0"),
            getNameReshId(
              registryName = map_db_resh$orgname[map_db_resh$UnitId == 0],
              reshId = 0,
              asNamedList = TRUE))

  org <- autoReportOrgServer(
    "smerteDispatchment",
    getNameReshId(registryName = req(registryName()),
                          reshId = req(user$org()),
                          asNamedList = TRUE))
  observeEvent(user$org(), {
    org <- autoReportOrgServer(
      "smerteDispatchment",
      getNameReshId(registryName = req(registryName()),
                            reshId = req(user$org()),
                            asNamedList = TRUE))
  }
  )
  org2 <- autoReportOrgServer("smerteDispatchmentNasjonal", orgs)

  vis_rapp <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  ## set reactive parameters overriding those in the reports list
  disParamNames <- reactive(c("registryName", "orgName", "orgId",
                                     "userRole", "outputType"))
  disParamValues <- reactive(c(registryName(), hospitalName(), org$value(),
                                      user$role(), format()))
  disParamValues2 <- reactive(c(registryName(), hospitalName(), org2$value(),
                                       user$role(), format2()))

  autoReportServer(
    "smerteDispatchment",
    registryName = "smerte",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = localReports,
    orgs = orgs,
    eligible = vis_rapp,
    freq = "quarter",
    user = user
  )

  autoReportServer(
    "smerteDispatchmentNasjonal",
    registryName = "smerte",
    type = "dispatchment",
    org = org2$value,
    paramNames = disParamNames,
    paramValues = disParamValues2,
    reports = nationalReports,
    orgs = orgs,
    eligible = vis_rapp,
    freq = "quarter",
    user = user
  )


  # Metadata
  meta <- reactive({
    describeRegistryDb(registryName())
  })

  output$metaControl <- renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })

  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$metaData <- renderUI({
    dataTableOutput("metaDataTable")
  })

  dumps = c("allevarnum", "smertediagnosernum", "smertediagnoser",
            "patient", "emp11", "emp_11_pain_diagnosis",
            "emp12", "emp22", "hads",
            "mce", "opiodoppf", "pateval", "patreg"
            )

  # Datadump
  output$dumpTabControl <- renderUI({
    selectInput("dumpDataSet", "Velg datasett:", dumps)
  })

  output$dumpDataInfo <- renderUI({
    p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file, input$dumpFormat, userRole = user$role())
    }
  )

  # Eksport
  ## brukerkontroller
  exportUCServer(
    "smerteExport", registryName, "smerte", eligible = req(vis_rapp)
  )

  ## veileding
  exportGuideServer2("smerteExportGuide", registryName)

  # Bruksstatistikk
  statsServer2("smerteStats", registryName = "smerte",
                        app_id = Sys.getenv("FALK_APP_ID"),
                        eligible = req(vis_rapp))
  statsGuideServer("smerteStats", registryName = "smerte")
}
