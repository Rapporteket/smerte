server <- function(input, output, session) {

  rapbase::appLogger(session, msg = "Starting smerte app")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    registryName <- smerte::makeRegistryName("smerte", reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    hospitalName <- smerte::getHospitalName(registryName, reshId, userRole)
    author <- userFullName
  } else {
    ### if need be, define your (local) values here
    hospitalName <- "Helse Bergen HF"
    reshId <- "100082"
  }

  # Hide tabs depending on context
  ## do not show local reports in national context
  if (smerte::isNationalReg(reshId)) {
    shiny::hideTab(inputId = "tabs", target = "Tilsyn")
    shiny::hideTab(inputId = "tabs", target = "Dekningsgrad")
    shiny::hideTab(inputId = "tabs", target = "Spinalkateter")
    shiny::hideTab(inputId = "tabs", target = "Smertekategori")
  }
  ## tools only for SC
  if (!userRole %in% "SC") {
    #shiny::hideTab(inputId = "tabs", target = "Verktøy")
    print("Please revert me!")
  }


  contentDump <- function(file, type) {
    d <- smerte::getDataDump(registryName,input$dumpDataSet,
                            fromDate = input$dumpDateRange[1],
                            toDate = input$dumpDateRange[2],
                            session = session)
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }

  # widget
  output$appUserName <- shiny::renderText(rapbase::getUserFullName(session))
  output$appOrgName <- shiny::renderText(
    paste(hospitalName, rapbase::getUserRole(session), sep = ", ")
  )

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = "smerte")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE,
                           confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- shiny::renderUI({
    #htmlRenderRmd("veiledning.Rmd")
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "smerte"),
      outputType = "html_fragment"
    )
  })

  reportParams <- list(
    hospitalName = hospitalName,
    reshId = reshId,
    registryName = registryName,
    userRole = userRole,
    userFullName = userFullName,
    shinySession = session
  )

  # Tilsynsrapport
  smerte::defaultReportServer(id = "tilsyn",
                              reportFileName = "LokalTilsynsrapportMaaned.Rmd",
                              reportParams = reportParams
  )

  # Dekningsgrad
  smerte::defaultReportServer(id = "dekningsgrad",
                              reportFileName = "LokalDekningsgradrapport.Rmd",
                              reportParams = reportParams)

  # Indikatorrapport
  reportTemplate <- "LokalIndikatorMaaned.Rmd"
  if (smerte::isNationalReg(reshId)) {
    reportTemplate <- "NasjonalIndikatorMaaned.Rmd"
  }
  smerte::defaultReportServer(id = "indikator",
                              reportFileName = reportTemplate,
                              reportParams = reportParams)

  # eProm
  smerte::defaultReportServer(id = "eprom",
                              reportFileName = "lokalEprom.Rmd",
                              reportParams = reportParams)

  # Spinalkateter
  smerte::defaultReportServer(id = "spinalkateter",
                              reportFileName = "LokalSpinalkateter.Rmd",
                              reportParams = reportParams)

  # Smertekategori
  smerte::defaultReportServer(id = "smertekategori",
                              reportFileName = "LokalSmertekategori.Rmd",
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
                      "Smerteregisteret", hospitalName, reshId,
                      registryName, userFullName)
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
                      hospitalName, reshId, registryName, userFullName,
                      userRole)
    ),
    `Kvalitetsindikatorer - lokal enhet` = list(
      synopsis = paste("Kvalitetsindikatorer fra Smerteregisteret",
                       "(lokal enhet)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title",
                     "author", "orgName", "orgId",
                     "userFullName", "userRole", "registryName"),
      paramValues = c("indikator", "pdf", "Kvalitetsindikatorer",
                      "Smerteregisteret", hospitalName, reshId,
                      userFullName, userRole, registryName)
    ),
    `Spinalkateter - lokal enhet` = list(
      synopsis = paste("Smerteregisteret: bruk av spinalkateter inneværende",
                       "år (lokal enhet)"),
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title",
                     "author", "orgId", "userFullName", "userRole",
                     "registryName", "orgName"),
      paramValues = c("spinalkateter", "pdf", "Spinalkateter",
                      "Smerteregisteret", reshId, userFullName, userRole,
                      registryName, hospitalName)
    )
  )

  orgs <- smerte::getNameReshId(registryName = registryName, asNamedList = TRUE)

  if (smerte::isNationalReg(reshId)) {
    orgs <- c(list(`Alle nasjonale data` = "0"), orgs)
    subReports <- nationalReports
    disReports <- c(nationalReports)
  } else {
    subReports <- localReports
    disReports <- localReports
  }

  # Abonnement
  rapbase::autoReportServer("smerteSubscription", registryName = "smerte",
                            type = "subscription", reports = subReports,
                            orgs = orgs)

  # Utsendelser
  format <- rapbase::autoReportFormatServer("smerteDispatchment")
  org <- rapbase::autoReportOrgServer("smerteDispatchment", orgs)

  ## set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive(c("orgName", "orgId", "outputType"))
  paramValues <- shiny::reactive(c(org$name(), org$value(), format()))

  rapbase::autoReportServer(
    "smerteDispatchment", registryName = "smerte", type = "dispatchment",
    org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = disReports, orgs = orgs, eligible = (userRole == "SC")
  )

  # Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb(registryName)
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })

  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  # Datadump
  output$dumpTabControl <- shiny::renderUI({
    selectInput("dumpDataSet", "Velg datasett:", names(meta()))
  })

  output$dumpDataInfo <- shiny::renderUI({
    p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file, input$dumpFormat)
    }
  )

  # Eksport
  ## brukerkontroller
  rapbase::exportUCServer("smerteExport", registryName = registryName,
                          repoName = "smerte", eligible = (userRole == "SC"))
  ## veileding
  rapbase::exportGuideServer("smerteExportGuide", registryName)

  # Bruksstatistikk
  rapbase::statsServer("smerteStats", registryName = "smerte",
                       eligible = TRUE) #(userRole == "SC")
  rapbase::statsGuideServer("smerteStats", registryName = "smerte")
}
