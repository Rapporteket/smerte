library(shiny)
library(shinyalert)
library(magrittr)
library(rapbase)
library(smerte)

server <- function(input, output, session) {

  raplog::appLogger(session)

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    hospitalName <- getHospitalName(reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    author <- paste0(userFullName, "/", "Rapporteket")
  } else {
    ### if need be, define your (local) values here
    hospitalName <- "Helse Bergen HF"
    reshId <- "100082"
  }

  # Hide tabs depending on context
  ## do now show local reports in national context
  if (isNationalReg(reshId)) {
    hideTab(inputId = "tabs", target = "Tilsynsrapport")
  }



  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    if (rapbase::isRapContext()) {
      params <- params
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
      registryName=makeRegistryName(baseName = "smerte", reshID = reshId),
      author=author
    ), output_dir = tempdir())
    file.rename(out, file)
  }



  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE,
               confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })

  # Tilsynsrapport
  output$years <- renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- getLocalYears(registryName = "smerte",
                             reshId = rapbase::getUserReshId(session))
      # remove NAs if they exists (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2016", "2017", "2018", "2019")
    }
    selectInput("yearSet", "Velg år:", years)
  })
  output$tilsynsrapport <- renderUI({
    reshId <- rapbase::getUserReshId(session)
    registryName <- makeRegistryName(baseName = "smerte", reshID = reshId)
    if (is.null(input$yearSet)) {
      NULL
    } else {
      htmlRenderRmd(srcFile = "LokalTilsynsrapportMaaned.Rmd",
                    params = list(hospitalName=hospitalName,
                                  year=input$yearSet,
                                  tableFormat='html',
                                  registryName=registryName)
      )
    }
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


  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordering = FALSE), rownames = FALSE
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
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
}
