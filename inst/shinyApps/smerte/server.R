library(shiny)
library(shinyalert)
library(magrittr)
library(rapbase)
library(smerte)

server <- function(input, output, session) {

  rapbase::appLogger(session, msg = "Starting smerte app")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    registryName <- makeRegistryName("smerte", reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    hospitalName <- getHospitalName(registryName, reshId, userRole)
    author <- userFullName
  } else {
    ### if need be, define your (local) values here
    hospitalName <- "Helse Bergen HF"
    reshId <- "100082"
  }

  # Hide tabs depending on context
  ## do not show local reports in national context
  if (isNationalReg(reshId)) {
    hideTab(inputId = "tabs", target = "Dekningsgrad")
    hideTab(inputId = "tabs", target = "Eprom")
    hideTab(inputId = "tabs", target = "Spinalkateter")
    hideTab(inputId = "tabs", target = "Smertekategori")
  }
  ## tools only for SC
  if (!userRole %in% "SC") {
    hideTab(inputId = "tabs", target = "Verktøy")
  }


  # filename function for re-use
  downloadFilename <- function(fileBaseName, type) {
    paste(paste0(fileBaseName,
                 as.character(as.integer(as.POSIXct(Sys.time())))),
          sep = '.', switch(
            type,
            PDF = 'pdf', HTML = 'html', REVEAL = 'html', BEAMER = 'pdf')
    )
  }

  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type, addParam = list()) {
    src <- normalizePath(system.file(srcFile, package="smerte"))
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    print(getwd())
    file.copy(src, tmpFile, overwrite = TRUE)
    file.copy(system.file("_bookdown.yml", package="smerte"), ".")
    file.copy(system.file("_output.yml", package="smerte"), ".")
    file.copy(system.file("rapporteket.cls", package="smerte"), ".")
    file.copy(system.file("preamble.tex", package="smerte"), ".")
    file.copy(system.file("www/logo_rapporteket_gray60.pdf",
                          package="smerte"), "logo.pdf")
    file.copy(system.file("www/logo_smerte.pdf", package="smerte"), ".")
    out <- rmarkdown::render(
      tmpFile,
      output_format =
        switch(
          type,
          PDF = bookdown::pdf_document2(pandoc_args = c("--include-in-header=preamble.tex")),
          #PDF = bookdown::pdf_document2(pandoc_args = c("--template=preamble.tex")),
          HTML = bookdown::html_document2(),
          BEAMER = rmarkdown::beamer_presentation(theme = "Hannover"),
          REVEAL = revealjs::revealjs_presentation(theme = "sky")
        ),
      params = c(
        list(tableFormat =
               switch(
                 type,
                 PDF = "latex",
                 HTML = "html",
                 BEAMER = "latex",
                 REVEAL = "html"),
             hospitalName=hospitalName,
             reshId=reshId,
             userRole=userRole,
             registryName=registryName,
             author=author,
             shinySession=session), addParam),
      output_dir = tempdir())
    file.rename(out, file)
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
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(paste(hospitalName,
                                  getUserRole(session), sep = ", "))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = "smerte")
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE,
               confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- renderUI({
    #htmlRenderRmd("veiledning.Rmd")
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "smerte"),
      outputType = "html_fragment"
    )
  })

  # Tilsynsrapport
  output$years <- renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- getLocalYears(registryName, reshId, userRole)
      # remove NAs if they exists (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2016", "2017", "2018", "2019", "2020")
    }
    selectInput("yearSet", "Velg år:", years)
  })
  output$tilsynsrapport <- renderUI({
    if (is.null(input$yearSet)) {
      NULL
    } else {
      rapbase::renderRmd(
        system.file("LokalTilsynsrapportMaaned.Rmd", package = "smerte"),
        outputType = "html_fragment",
        params = list(hospitalName=hospitalName,
                      year=input$yearSet,
                      tableFormat='html',
                      registryName=registryName,
                      reshId=reshId,
                      userRole=userRole,
                      shinySession=session)
      )
    }
  })

  output$downloadReportTilsyn <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "LokalTilsynsrapportMaaned",
                        fileext = paste0(".", input$formatTilsyn)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("LokalTilsynsrapportMaaned.Rmd", package = "smerte"),
        outputType = input$formatTilsyn,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatTilsyn,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      year = input$yearSet,
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )


  # Dekningsgrad
  output$dekningsgrad <- renderUI({
    rapbase::renderRmd(
      system.file("LokalDekningsgradrapport.Rmd", package = "smerte"),
      outputType = "html_fragment",
      params = list(hospitalName=hospitalName,
                    reshId=reshId,
                    startDate=input$dateRangeDekningsgrad[1],
                    endDate=input$dateRangeDekningsgrad[2],
                    tableFormat='html',
                    registryName=registryName,
                    userRole=userRole,
                    userFullName = userFullName,
                    shinySession=session)
    )
  })

  output$downloadReportDekningsgrad <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern ="LokalDekningsgradrapport",
                        fileext = paste0(".", input$formatDekningsgrad)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("LokalDekningsgradrapport.Rmd", package = "smerte"),
        outputType = input$formatDekningsgrad,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatDekningsgrad,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      startDate=input$dateRangeDekningsgrad[1],
                      endDate=input$dateRangeDekningsgrad[2],
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )


  # Indikatorrapport
  output$indYears <- renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- getAllYears(registryName, reshId, userRole)
      # remove NAs if they exists (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2016", "2017", "2018", "2019", "2020")
    }
    selectInput("indYearSet", "Velg år:", years)
  })
  output$indikatorrapport <- renderUI({
    reportTemplate <- "LokalIndikatorMaaned.Rmd"
    if (isNationalReg(reshId)) {
      reportTemplate <- "NasjonalIndikatorMaaned.Rmd"
    }
    if (is.null(input$indYearSet)) {
      p("Velg fra menyen til venstre hvilket år indikatorene skal vises for.")
    } else {
      rapbase::renderRmd(
        system.file(reportTemplate, package = "smerte"),
        outputType = "html_fragment",
        params = list(hospitalName=hospitalName,
                      year=input$indYearSet,
                      tableFormat='html',
                      registryName=registryName,
                      reshId=reshId,
                      userRole=userRole,
                      shinySession=session)
      )
    }
  })

  output$downloadReportIndikator <- downloadHandler(
    filename = function() {
      repPrefix <- "Lokal"
      if (isNationalReg(reshId)) {
        repPrefix <- "Nasjonal"
      }
      basename(tempfile(pattern = paste0(repPrefix, "IndikatorMaaned"),
                        fileext = paste0(".", input$formatIndikator)))
    },
    content = function(file) {
      repPrefix <- "Lokal"
      if (isNationalReg(reshId)) {
        repPrefix <- "Nasjonal"
      }
      fn <- rapbase::renderRmd(
        system.file(paste0(repPrefix, "IndikatorMaaned.Rmd"),
                    package = "smerte"),
        outputType = input$formatIndikator,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatIndikator,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      year=input$indYearSet,
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )

  # eProm
  output$eprom <- renderUI({
    rapbase::renderRmd(
      system.file("lokalEprom.Rmd", package = "smerte"),
      outputType = "html_fragment",
      params = list(hospitalName=hospitalName,
                    reshId=reshId,
                    startDate=input$dateRangeEprom[1],
                    endDate=input$dateRangeEprom[2],
                    tableFormat = "html",
                    registryName=registryName,
                    userRole=userRole,
                    shinySession=session)
    )
  })

  output$downloadReportEprom <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern ="lokalEprom",
                        fileext = paste0(".", input$formatEprom)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("lokalEprom.Rmd", package = "smerte"),
        outputType = input$formatEprom,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatEprom,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      startDate=input$dateRangeEprom[1],
                      endDate=input$dateRangeEprom[2],
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )

  # Spinalkateter
  output$spinalkateter <- renderUI({
    rapbase::renderRmd(
      system.file("LokalSpinalkateter.Rmd", package = "smerte"),
      outputType = "html_fragment",
      params = list(hospitalName=hospitalName,
                    reshId=reshId,
                    startDate=input$dateRangeSpinalkateter[1],
                    endDate=input$dateRangeSpinalkateter[2],
                    tableFormat = "html",
                    registryName=registryName,
                    userRole=userRole,
                    shinySession=session)
    )
  })

  output$downloadReportSpinalkateter <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern ="LokalSpinalkateter",
                        fileext = paste0(".", input$formatSpinalkateter)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("LokalSpinalkateter.Rmd", package = "smerte"),
        outputType = input$formatSpinalkateter,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatSpinalkateter,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      startDate=input$dateRangeSpinalkateter[1],
                      endDate=input$dateRangeSpinalkateter[2],
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )

  # Smertekategori
  output$smertekategori <- renderUI({
    rapbase::renderRmd(
      system.file("LokalSmertekategori.Rmd", package = "smerte"),
      outputType = "html_fragment",
      params = list(hospitalName=hospitalName,
                    reshId=reshId,
                    startDate=input$dateRangeSmertekategori[1],
                    endDate=input$dateRangeSmertekategori[2],
                    tableFormat = "html",
                    registryName=registryName,
                    userRole=userRole,
                    shinySession=session)
    )
  })

  output$downloadReportSmertekategori <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern ="LokalSmertekategori",
                        fileext = paste0(".", input$formatSmertekategori)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("LokalSmertekategori.Rmd", package = "smerte"),
        outputType = input$formatSmertekategori,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatSmertekategori,
                      reshId = reshId,
                      registryName = registryName,
                      userRole = userRole,
                      userFullName = userFullName,
                      startDate=input$dateRangeSmertekategori[1],
                      endDate=input$dateRangeSmertekategori[2],
                      shinySession = session)
      )
      file.rename(fn, file)
    }
  )

  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeAutoReportTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordering = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = 6))),
    rownames = FALSE
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
                 ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  ### lag liste over mulige valg styrt av lokal eller nasjonal sesjon
  output$subscriptionRepList <- renderUI({
    if (isNationalReg(reshId)) {
      selectInput("subscriptionRep", "Rapport:",
                  c(""))
    } else {
      selectInput("subscriptionRep", "Rapport:",
                  c("Lokalt tilsyn per måned 2016",
                    "Lokalt tilsyn per måned 2017"))
    }
  })

  observeEvent (input$subscribe, {
    if (nchar(input$subscriptionRep) > 0) {

      package <- "smerte"
      owner <- rapbase::getUserName(session)
      interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
      intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
      organization <- rapbase::getUserReshId(session)
      runDayOfYear <- rapbase::makeRunDayOfYearSequence(
        interval = interval)
      email <- rapbase::getUserEmail(session)
      synopsis <- "Rutinemessig utsending av lokal tilsynsrapport"
      baseName <- "LokalTilsynsrapportMaaned"
      registryName <- makeRegistryName(baseName = "smerte", reshID = reshId)
      fun <- "subscriptionLocalTilsyn"
      if (input$subscriptionRep == "Lokalt tilsyn per måned 2016") {
        year <- "2016"
      }
      if (input$subscriptionRep == "Lokalt tilsyn per måned 2017") {
        year <- "2017"
      }
      paramNames <- c("baseName", "reshId", "registryName", "author",
                      "hospitalName", "year", "type")
      paramValues <- c(baseName, reshId, registryName, author, hospitalName,
                       year, input$subscriptionFileFormat)
      rapbase::createAutoReport(synopsis = synopsis, package = package,
                                fun = fun, paramNames = paramNames,
                                paramValues = paramValues, owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear,
                                interval = interval, intervalName = intervalName)
    }
    rv$subscriptionTab <- rapbase::makeAutoReportTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeAutoReportTab(session)
  })

  # Abonnement (NY)
  subReports <- list(
    Tilsyn = list(
      synopsis = "Tilsynsrapport",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "orgId"),
      paramValues = c("tilsyn", "pdf", "Tilsyn", reshId)
    ),
    Spinalkateter = list(
      synopsis = "Spinalkateterrapport",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "orgId"),
      paramValues = c("spinalkateter", "pdf", "Spinalkateter", reshId)
    )
  )
  rapbase::autoReportFormatServer("smerteSubscription")
  rapbase::autoReportServer("smerteSubscription", registryName = "smerte",
                            type = "subscription", reports = subReports)

  # Metadata
  meta <- reactive({
    smerte::describeRegistryDb(registryName)
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
    DT::dataTableOutput("metaDataTable")
  })

  # Datadump
  output$dumpTabControl <- renderUI({
    selectInput("dumpDataSet", "Velg datasett:", names(meta()))
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
                       eligible = (userRole == "SC"))
  rapbase::statsGuideServer("smerteStats", registryName = "smerte")
}
