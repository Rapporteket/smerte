server <- function(input, output, session) {

  # rapbase::logShinyInputChanges(input)

  map_db_resh <-
    rapbase::getConfig("rapbaseConfig.yml")$reg$smerte$databases |>
    unlist() |>
    matrix(nrow=2) |>
    t() |>
    as.data.frame() |>
    dplyr::rename(orgname = V1, UnitId = V2)

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "smerte",
    caller = "smerte",
    map_orgname = shiny::req(map_db_resh)
  )

  rapbase::appLogger(session, msg = "Starting smerte app")

  # Parameters that may change depending on the role and org of user
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    registryName <- reactive(
      map_db_resh$orgname[map_db_resh$UnitId == user$org()])
    userFullName <- Sys.getenv("FALK_USER_FULLNAME")
    hospitalName <- reactive(smerte::getHospitalName(
      registryName(),
      user$org(),
      user$role())
    )
  }

  # Hide tabs depending on context
  ## do not show local reports in national context
  shiny::observeEvent(user$org(), {
    if (smerte::isNationalReg(shiny::req(user$org()))) {
      shiny::hideTab(inputId = "tabs", target = "Tilsyn")
      shiny::hideTab(inputId = "tabs", target = "Dekningsgrad før reservasjon")
      shiny::hideTab(inputId = "tabs", target = "Dekningsgrad etter reservasjon")
      shiny::hideTab(inputId = "tabs", target = "Spinalkateter")
      shiny::hideTab(inputId = "tabs", target = "Smertekategori")
      shiny::hideTab(inputId = "tabs", target = "Oppfølging ved smerteklinikk")
      shiny::hideTab(inputId = "tabs", target = "Epidural (barn)")
      shiny::hideTab(inputId = "tabs", target = "Abonnement lokal")
      shiny::showTab(inputId = "tabs", target = "Abonnement nasjonal")
    }
    if (!(smerte::isNationalReg(shiny::req(user$org())))) {
      shiny::showTab(inputId = "tabs", target = "Tilsyn")
      shiny::showTab(inputId = "tabs", target = "Dekningsgrad før reservasjon")
      shiny::showTab(inputId = "tabs", target = "Dekningsgrad etter reservasjon")
      shiny::showTab(inputId = "tabs", target = "Spinalkateter")
      shiny::showTab(inputId = "tabs", target = "Smertekategori")
      shiny::showTab(inputId = "tabs", target = "Oppfølging ved smerteklinikk")
      shiny::showTab(inputId = "tabs", target = "Abonnement lokal")
      shiny::hideTab(inputId = "tabs", target = "Abonnement nasjonal")
    }
  }
  )

  ## tools only for SC
  observeEvent(list(user$role(), user$org()), {
    if (!shiny::req(user$role()) %in% "SC") {
      shiny::hideTab(inputId = "tabs", target = "Verktøy")
    }
    if (shiny::req(user$role()) %in% "SC") {
      shiny::showTab(inputId = "tabs", target = "Verktøy")
      if (smerte::isNationalReg(shiny::req(user$org()))) {
        shiny::showTab(inputId = "tabs", target = "Utsendelser nasjonal")
        shiny::hideTab(inputId = "tabs", target = "Utsendelser")
      } else {
        shiny::showTab(inputId = "tabs", target = "Utsendelser")
        shiny::hideTab(inputId = "tabs", target = "Utsendelser nasjonal")
      }

    }
  }
  )

  contentDump <- function(file, type, userRole = "LU") {
    d <- smerte::getDataDump(registryName(),input$dumpDataSet,
                             reshId = user$org(),
                             fromDate = input$dumpDateRange[1],
                             toDate = input$dumpDateRange[2],
                             session = session)
    if (userRole == "LU") {
      if (input$dumpDataSet %in% c("SmerteDiagnoser", "smertediagnosernum")) {
        forlopsoversikt <- rapbase::loadRegData(
          registryName(),
          "SELECT ForlopsID, AvdRESH FROM forlopsoversikt")
        d <- merge(d, forlopsoversikt, by = "ForlopsID")
      }
      if (input$dumpDataSet != "avdelingsoversikt") {
        d <- dplyr::filter(d, AvdRESH == shiny::req(user$org()))
      }
    }
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }


  # Veiledning
  output$veiledning <- shiny::renderUI({
    #htmlRenderRmd("veiledning.Rmd")
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "smerte"),
      outputType = "html_fragment"
    )
  })

  reportParams <- shiny::reactive(
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
  smerte::defaultReportServer2(
    id = "tilsyn",
    reportFileName = reactiveVal("LokalTilsynsrapportMaaned.Rmd"),
    reportParams = reportParams
  )

  # Dekningsgrad gammel
  smerte::defaultReportServer2(
    id = "dekningsgrad",
    reportFileName = reactiveVal("LokalDekningsgradrapport.Rmd"),
    reportParams = reportParams)
  # Dekningsgrad ny
  smerte::defaultReportServer2(
    id = "dekningsgradReserv",
    reportFileName = reactiveVal("LokalDekningsgradrapportReservasjon.Rmd"),
    reportParams = reportParams)

  # Indikatorrapport
  reportTemplate <- shiny::reactiveVal()
  observeEvent(user$org(), {
    if (smerte::isNationalReg(user$org())) {
      reportTemplate("NasjonalIndikatorMaaned.Rmd")
    } else {
      reportTemplate("LokalIndikatorMaaned.Rmd")
    }
  }
  )

  smerte::defaultReportServer2(
    id = "indikator",
    reportFileName = reportTemplate,
    reportParams = reportParams)

  # Opiodreduksjon

  reportTemplate2 <- shiny::reactiveVal()
  observeEvent(user$org(), {
    if (smerte::isNationalReg(user$org())) {
      reportTemplate2("NasjonalOpioidReduksjon.Rmd")
    } else {
      reportTemplate2("LokalOpioidReduksjon.Rmd")
    }
  }
  )

  smerte::defaultReportServer2(
    id = "opioid",
    reportFileName = reportTemplate2,
    reportParams = reportParams)


  # eProm
  smerte::defaultReportServer2(
    id = "eprom",
    reportFileName = reactiveVal("lokalEprom.Rmd"),
    reportParams = reportParams)

  # Spinalkateter
  smerte::defaultReportServer2(
    id = "spinalkateter",
    reportFileName = reactiveVal("LokalSpinalkateter.Rmd"),
    reportParams = reportParams)

  # Smertekategori
  smerte::defaultReportServer2(
    id = "smertekategori",
    reportFileName = reactiveVal("LokalSmertekategori.Rmd"),
    reportParams = reportParams)

  # Tid til død
  smerte::defaultReportServer2(
    id = "timetodeath",
    reportFileName = reactiveVal("timetodeath.Rmd"),
    reportParams = reportParams)

  # Oppfølging ved smerteklinikk
  smerte::defaultReportServer2(
    id = "oppfolg",
    reportFileName = reactiveVal("LokalOppfolg.Rmd"),
    reportParams = reportParams)
  # Epidural hos barn
  smerte::defaultReportServer2(
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
  subParamNames <- shiny::reactive(c("registryName", "orgName", "orgId",
                                     "userRole"))
  subParamValues <- shiny::reactive(c(registryName(), hospitalName(), user$org(),
                                      user$role()))

  # Abonnement

  rapbase::autoReportServer(
    "smerteSubscription",
    registryName = "smerte",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = localReports,
    freq = "quarter",
    user = user
  )

  rapbase::autoReportServer(
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
  format <- rapbase::autoReportFormatServer("smerteDispatchment")
  format2 <- rapbase::autoReportFormatServer("smerteDispatchmentNasjonal")

  orgs <- c(list(`Alle nasjonale data` = "0"),
            smerte::getNameReshId(
              registryName = map_db_resh$orgname[map_db_resh$UnitId == 0],
              reshId = 0,
              asNamedList = TRUE))

  org <- rapbase::autoReportOrgServer(
    "smerteDispatchment",
    smerte::getNameReshId(registryName = shiny::req(registryName()),
                          reshId = shiny::req(user$org()),
                          asNamedList = TRUE))
  shiny::observeEvent(user$org(), {
    org <- rapbase::autoReportOrgServer(
      "smerteDispatchment",
      smerte::getNameReshId(registryName = shiny::req(registryName()),
                            reshId = shiny::req(user$org()),
                            asNamedList = TRUE))
  }
  )
  org2 <- rapbase::autoReportOrgServer("smerteDispatchmentNasjonal", orgs)

  vis_rapp <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  ## set reactive parameters overriding those in the reports list
  disParamNames <- shiny::reactive(c("registryName", "orgName", "orgId",
                                     "userRole", "outputType"))
  disParamValues <- shiny::reactive(c(registryName(), hospitalName(), org$value(),
                                      user$role(), format()))
  disParamValues2 <- shiny::reactive(c(registryName(), hospitalName(), org2$value(),
                                       user$role(), format2()))

  rapbase::autoReportServer(
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

  rapbase::autoReportServer(
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
  meta <- shiny::reactive({
    rapbase::describeRegistryDb(registryName())
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
      contentDump(file, input$dumpFormat, userRole = user$role())
    }
  )

  # Eksport
  ## brukerkontroller
  rapbase::exportUCServer2(
    "smerteExport", registryName = registryName,
    repoName = "smerte", eligible = shiny::req(vis_rapp))

  ## veileding
  rapbase::exportGuideServer2("smerteExportGuide", registryName)

  # Bruksstatistikk
  rapbase::statsServer2("smerteStats", registryName = "smerte",
                        eligible = shiny::req(vis_rapp))
  rapbase::statsGuideServer("smerteStats", registryName = "smerte")
}
