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
    registryName <- reactive(map_db_resh$orgname[map_db_resh$UnitId == user$org()])
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
    }
    if (!(smerte::isNationalReg(shiny::req(user$org())))) {
      shiny::showTab(inputId = "tabs", target = "Tilsyn")
      shiny::showTab(inputId = "tabs", target = "Dekningsgrad før reservasjon")
      shiny::showTab(inputId = "tabs", target = "Dekningsgrad etter reservasjon")
      shiny::showTab(inputId = "tabs", target = "Spinalkateter")
      shiny::showTab(inputId = "tabs", target = "Smertekategori")
      shiny::showTab(inputId = "tabs", target = "Oppfølging ved smerteklinikk")
      shiny::showTab(inputId = "tabs", target = "Epidural (barn)")
    }
  }
  )

  ## tools only for SC
  observe({
    if (!shiny::req(user$role()) %in% "SC") {
      shiny::hideTab(inputId = "tabs", target = "Verktøy")
    }
    if (shiny::req(user$role()) %in% "SC") {
      shiny::showTab(inputId = "tabs", target = "Verktøy")
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
      if (input$dumpDataSet %in% c("SmerteDiagnoser", "SmerteDiagnoserNum")) {
        ForlopsOversikt <- rapbase::loadRegData(
          registryName(),
          "SELECT ForlopsID, AvdRESH FROM ForlopsOversikt")
        d <- merge(d, ForlopsOversikt, by = "ForlopsID")
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

  reportParams <- reactive(
    list(
      hospitalName = hospitalName(),
      reshId = user$org(),
      registryName = registryName(),
      userRole = user$role(),
      userFullName = userFullName,
      shinySession = session
    ))

  # # Tilsynsrapport
  smerte::defaultReportServer(id = "tilsyn",
                              reportFileName = "LokalTilsynsrapportMaaned.Rmd",
                              reportParams = reportParams()
  )

  # Dekningsgrad gammel
  smerte::defaultReportServer(id = "dekningsgrad",
                              reportFileName = "LokalDekningsgradrapport.Rmd",
                              reportParams = reportParams())
  # Dekningsgrad ny
  smerte::defaultReportServer(id = "dekningsgradReserv",
                              reportFileName = "LokalDekningsgradrapportReservasjon.Rmd",
                              reportParams = reportParams())

  # Indikatorrapport
  reportTemplate <- "LokalIndikatorMaaned.Rmd"
  observe(
    if (smerte::isNationalReg(user$org())) {
      reportTemplate <- "NasjonalIndikatorMaaned.Rmd"
    })
  smerte::defaultReportServer(id = "indikator",
                              reportFileName = reportTemplate,
                              reportParams = reportParams())

  # Opiodreduksjon
  reportTemplate2 <- "LokalOpioidReduksjon.Rmd"
  observe(
    if (smerte::isNationalReg(user$org())) {
      reportTemplate2 <- "NasjonalOpioidReduksjon.Rmd"
    })
  smerte::defaultReportServer(id = "opioid",
                              reportFileName = reportTemplate2,
                              reportParams = reportParams())


  # eProm
  smerte::defaultReportServer(id = "eprom",
                              reportFileName = "lokalEprom.Rmd",
                              reportParams = reportParams())

  # Spinalkateter
  smerte::defaultReportServer(id = "spinalkateter",
                              reportFileName = "LokalSpinalkateter.Rmd",
                              reportParams = reportParams())

  # Smertekategori
  smerte::defaultReportServer(id = "smertekategori",
                              reportFileName = "LokalSmertekategori.Rmd",
                              reportParams = reportParams())

  # Tid til død
  smerte::defaultReportServer(id = "timetodeath",
                              reportFileName = "timetodeath.Rmd",
                              reportParams = reportParams())

  # Oppfølging ved smerteklinikk
  smerte::defaultReportServer(id = "oppfolg",
                              reportFileName = "LokalOppfolg.Rmd",
                              reportParams = reportParams())
  # Epidural hos barn
  smerte::defaultReportServer(id = "lokalepi",
                              reportFileName = "LokalEpidural.Rmd",
                              reportParams = reportParams())

  # Definisjon av rapporter for abonnement og utsendelser

  rapporter <-
    shiny::reactive(
      if (smerte::isNationalReg(user$org())) {
        list(
          `Kvalitetsindikatorer - alle enheter`= list(
            synopsis = paste("Kvalitetsindikatorer fra Smerteregisteret",
                             "(alle enheter)"),
            fun = "reportProcessor",
            paramNames = c("report", "outputType", "title",
                           "author", "orgName", "orgId",
                           "registryName", "userFullName"),
            paramValues = c("nasjonalIndikator", "pdf", "Kvalitetsindikatorer",
                            "Smerteregisteret", hospitalName(), user$org(),
                            registryName(), userFullName)
          )
        )
      } else {
        list(
          `Tilsyn - lokal enhet` = list(
            synopsis = paste("Smerteregisteret: månedlig oppsummering av tilsyn",
                             "siste år (lokal enhet)"),
            fun = "reportProcessor",
            paramNames = c("report", "outputType", "title", "author",
                           "orgName", "orgId", "registryName", "userFullName",
                           "userRole"),
            paramValues = c("tilsyn", "pdf", "Tilsyn", "Smerteregisteret",
                            hospitalName(), user$org(), registryName(), userFullName,
                            user$role())
          ),
          `Kvalitetsindikatorer - lokal enhet` = list(
            synopsis = paste("Kvalitetsindikatorer fra Smerteregisteret",
                             "(lokal enhet)"),
            fun = "reportProcessor",
            paramNames = c("report", "outputType", "title",
                           "author", "orgName", "orgId",
                           "userFullName", "userRole", "registryName"),
            paramValues = c("indikator", "pdf", "Kvalitetsindikatorer",
                            "Smerteregisteret", hospitalName(), user$org(),
                            userFullName, user$role(), registryName())
          ),
          `Spinalkateter - lokal enhet` = list(
            synopsis = paste("Smerteregisteret: bruk av spinalkateter inneværende",
                             "år (lokal enhet)"),
            fun = "reportProcessor",
            paramNames = c("report", "outputType", "title",
                           "author", "orgId", "userFullName", "userRole",
                           "registryName", "orgName"),
            paramValues = c("spinalkateter", "pdf", "Spinalkateter",
                            "Smerteregisteret", user$org(), userFullName, user$role(),
                            registryName(), hospitalName())
          )
        )
      }
    )

  orgs <- shiny::reactive(
    if (smerte::isNationalReg(user$org())) {
      c(list(`Alle nasjonale data` = "0"),
        smerte::getNameReshId(registryName = registryName(),
                              reshId = user$org(),
                              asNamedList = TRUE))
    } else {
      smerte::getNameReshId(registryName = registryName(),
                            reshId = user$org(),
                            asNamedList = TRUE)
    }
  )

  # Abonnement
  observe(
    rapbase::autoReportServer2(
      "smerteSubscription",
      registryName = "smerte",
      type = "subscription",
      reports = shiny::req(rapporter()),
      orgs = shiny::req(orgs()),
      eligible = TRUE,
      freq = "quarter",
      user = user
    )
  )

  # # Utsendelser
  format <- rapbase::autoReportFormatServer("smerteDispatchment")

  # observeEvent(orgs(), {
    org <- rapbase::autoReportOrgServer("smerteDispatchment", orgs())
  # })


  ## set reactive parameters overriding those in the reports list
  # paramNames <- shiny::reactive(c("orgName", "orgId", "outputType"))
  # paramValues <- shiny::reactive(c(org$name(), org$value(), format()))
  #
  observe(
    rapbase::autoReportServer2(
      "smerteDispatchment",
      registryName = "smerte",
      type = "dispatchment",
      org = reactiveVal(c("")),
      reports = shiny::req(rapporter()),
      orgs = orgs(),
      eligible = (user$role() == "SC"),
      freq = "quarter",
      user = user
    )
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
  shiny::observe(
    rapbase::exportUCServer("smerteExport", registryName = registryName(),
                            repoName = "smerte", eligible = (shiny::req(user$role()) == "SC"))
  )
  ## veileding
  shiny::observe(
    rapbase::exportGuideServer("smerteExportGuide", registryName()))

  # Bruksstatistikk
  shiny::observe(
    rapbase::statsServer("smerteStats", registryName = "smerte",
                         eligible = (shiny::req(user$role()) == "SC")))
  rapbase::statsGuideServer("smerteStats", registryName = "smerte")
}
