#################################
## Oppsett som kjører hos SKDE ##
## --------------------------- ##
## HV må ha noe annet oppsett  ##
#################################

# Dekrypter en gjeng med databasedumper
# Få ut liste med `ls -tr` i bash når man står i Downloads-mappa
tarfiles <- c(
  "smerte_unn13f226394.sql.gz__20250624_150656.tar.gz",
  "smerte_nasjonal13635fd5.sql.gz__20250624_150712.tar.gz",
  "smerte_ahus12871168c.sql.gz__20250624_150728.tar.gz",
  "smerte_stolavs17bfc01a8.sql.gz__20250624_150748.tar.gz",
  "smerte_vestreviken159863cbd.sql.gz__20250624_150821.tar.gz",
  "smerte_bergen137d9dae7.sql.gz__20250624_150838.tar.gz",
  "smerte_levanger13d5be292.sql.gz__20250624_150851.tar.gz",
  "smerte_moreromsdal1b631f95.sql.gz__20250624_150903.tar.gz",
  "smerte_ous1155a0ec3.sql.gz__20250624_150915.tar.gz"
)
for (i in tarfiles) {
  sship::dec(
    paste0("c://Users/ast046/Downloads/", i),
    keyfile = "p://.ssh/id_rsa",
    target_dir = "c://Users/ast046/Downloads/."
  )
}


devtools::install("../rapbase/.", upgrade = FALSE)
devtools::install(upgrade = FALSE)


source("dev/Renv.R")
shiny::runApp('inst/shinyApps/smerte', launch.browser = TRUE)


