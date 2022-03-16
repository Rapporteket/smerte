## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "moduleTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
                      package = "rapbase"),
          Sys.getenv("R_RAP_CONFIG_PATH"))

registryName <- "smerte"

test_that("module input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(defaultReportInput("id")))
})

test_that("module UI returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(defaultReportUI("id")))
})

# prep arguments
reportParams = list(hospitalName = "Testsykehus",
                    reshId = 100082,
                    registryName = "Testregister",
                    userRole = "LU",
                    userFullName = "Tore Tester",
                    shinySession = list())

reportFileName <- "sampleReport.Rmd"

test_that("module server provides sensible output", {
  shiny::testServer(
    defaultReportServer,
    args = list(reportFileName = reportFileName,
                reportParams = reportParams), {
                  session$setInputs(dateRange = c(Sys.Date(), Sys.Date() + 1), format = "html")
                  expect_equal(class(output$report), "list")
                  session$setInputs(downloadReport = 1)
                  expect_true(file.exists(output$downloadReport))
  })
})

test_that("test app returns an app object", {
  expect_equal(class(defaultReportApp()), "shiny.appobj")
})


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
