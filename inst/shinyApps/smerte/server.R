library(shiny)
library(shinyalert)
library(magrittr)
library(rapbase)
library(smerte)

server <- function(input, output, session) {

  #raplog::appLogger(session)

  regData <- mtcars

  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    context <- Sys.getenv("R_RAP_INSTANCE")
    if (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) {
      params <- list(reshId=rapbase::getUserReshId(session),
                     year=input$yearSet,
                     tableFormat="html",
                     session = session)
    } else {
      params <- list(reshId="100082",
                     year=input$yearSet,
                     tableFormat="html")
    }
    system.file(srcFile, package="smerte") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code'),
                                            encoding = "utf-8") %>%
      shiny::HTML()
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
  contentFile <- function(file, srcFile, tmpFile, type) {
    src <- normalizePath(system.file(srcFile, package="smerte"))
    context <- Sys.getenv("R_RAP_INSTANCE")
    if (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) {
      hospitalName <-getHospitalName(rapbase::getUserReshId(session))
      reshId <- rapbase::getUserReshId(session)
    } else {
      hospitalName <- "Helse Bergen HF"
      reshId <- "100082"
    }

    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    library(rmarkdown)
    out <- render(tmpFile, output_format = switch(
      type,
      PDF = pdf_document(),
      HTML = html_document(),
      BEAMER = beamer_presentation(theme = "Hannover"),
      REVEAL = revealjs::revealjs_presentation(theme = "sky")
    ), params = list(tableFormat=switch(
      type,
      PDF = "latex",
      HTML = "html",
      BEAMER = "latex",
      REVEAL = "html"),
      hospitalName=hospitalName,
      reshId=reshId,
      year=input$yearSet,
      session=session
    ), output_dir = tempdir())
    # active garbage collection to prevent memory hogging?
    gc()
    file.rename(out, file)
  }



  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(getUserReshId(session))


  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })

  # Tilsynsrapport
  ## years available, hardcoded if outside known context
  if (Sys.getenv("R_RAP_INSTANCE") %in% c("DEV", "TEST", "QA", "PRODUCTION")) {
    years <- getLocalYears(registryName = "smerte",
                           reshId = rapbase::getUserReshId(session))[[1]]
    # remove NAs if they exists (bad registry)
    years <- years[!is.na(years)]
  } else {
    years <- c("2016", "2017", "2018", "2019")
  }

  output$years <- renderUI({
    selectInput("yearSet", "Velg år:", years)
  })
  output$tilsynsrapport <- renderUI({
    htmlRenderRmd("LokalTilsynsrapportMaaned.Rmd")
  })

  output$downloadReportTilsyn <- downloadHandler(
    filename = function() {
      downloadFilename("LokalTilsynsrapportMaaned",
                       input$formatTilsyn)
    },

    content = function(file) {
      contentFile(file, "LokalTilsynsrapportMaaned.Rmd",
                  "tmpLokalTilsynsrapportMaaned.Rmd",
                  input$formatTilsyn)
    }
  )

  # Figur og tabell
  ## Figur
  output$distPlot <- renderPlot({
    #raplog::repLogger(session, msg = "Test parent frame")
    makeHist(df = regData, var = input$var, bins = input$bins, makeTable = FALSE, session = session)
  })

  ## Tabell
  output$distTable <- renderTable({
    makeHist(df = regData, var = input$var, bins = input$bins, makeTable = TRUE, session = session)
  })

  # Sammendrag
  # output$distSummary <- renderTable({
  #   as.data.frame(sapply(regData, summary))[input$var]
  # }, rownames = TRUE)

  # Samlerapport
  ## vis
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = "samlerapport.Rmd",
                  params = list(var = input$varS, bins = input$binsS))
  })

  ## last ned
  output$downloadSamlerapport <- downloadHandler(
    filename = function() {
      "rapRegTemplateSamlerapport.html"
    },
    content = function(file) {
      srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                           package = "rapRegTemplate"))
      tmpFile <- "tmpSamlerapport.Rmd"
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(srcFile, tmpFile, overwrite = TRUE)
      out <- rmarkdown::render(tmpFile,
                               output_format =  rmarkdown::html_document(),
                               params = list(var = input$varS,
                                             bins = input$binsS),
                               output_dir = tempdir())
      file.rename(out, file)
    }
  )


  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp'), rownames = FALSE
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
                 " (", userEmail, "):")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  observeEvent (input$subscribe, {
    package <- "smerte"
    owner <- getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- rapbase::getUserEmail(session)
    organization <- rapbase::getUserReshId(session)

    if (input$subscriptionRep == "Lokalt tilsyn") {
      synopsis <- "Rutinemessig utsending av lokal tilsynsrapport"
      fun <- "lokalTilsynFun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)

    }
    if (input$subscriptionRep == "Nasjonalt tilsyn") {
      synopsis <- "Rutinemesig utsending av nasjonal tilsynsrapport"
      fun <- "nasjonaltTilsynFun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })


  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })
}
