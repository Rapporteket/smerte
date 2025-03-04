# devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE, dependencies = FALSE)
#
# Sys.setenv(R_RAP_INSTANCE="QAC")
# Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/mydata")
# Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":101,\"R\":\"LU\",\"U\":0},{\"A\":101,\"R\":\"SC\",\"U\":0},{\"A\":101,\"R\":\"SC\",\"U\":100089},{\"A\":101,\"R\":\"LU\",\"U\":100082}]")

shiny::runApp('inst/shinyApps/smerte')

# shiny::runApp(system.file("shinyApps/smerte", package = "smerte"))

# rapbase::runAutoReport()
# Rscript -e "Sys.setenv(R_RAP_INSTANCE=\"QAC\")" -e "rapbase::runAutoReport(dato = Sys.Date()+1, dryRun = TRUE)"

# d <- smerte::getDataDump("smertereportdatastaging",
#                          reshId = "100082",
#                          tableName = "allevarnum",
#                          fromDate = "2020-01-01",
#                          toDate = "2025-01-01")
#
# smerte::getHospitalName("smertereportdatastaging",
#                         reshId = "100082",
#                         userRole = "LU")
#
#
# tmp_yml <- yaml::read_yaml("./dev/test.yml")
# tmp_json <- jsonlite::serializeJSON(tmp_yml)
# query <- paste0("INSERT INTO `autoreport` VALUES ('", tmp_json, "');")
#
# con <- rapbase::rapOpenDbConnection("autoreport")$con
# DBI::dbExecute(con, query)
# rapbase::rapCloseDbConnection(con)
