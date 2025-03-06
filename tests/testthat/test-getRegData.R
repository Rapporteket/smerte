# For these tests to work locally make sure an instance of mysql server is
# available and that the necassary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# When run at Github Actions build servers [USER] must be set to 'actions' and
# with an empty password (as also assumed in the above localhost example).
# See also .github/workflows/R-CMD-check.yml

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# preserve initial state
config_path <- Sys.getenv("R_RAP_CONFIG_PATH")


test_that("env vars needed for testing is present", {
  check_db()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             host = Sys.getenv("DB_HOST"),
                             user = Sys.getenv("DB_USER"),
                             password = Sys.getenv("DB_PASS"),
                             bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testDb;")
  RMariaDB::dbDisconnect(con)
}

# make temporary config
test_config <- paste0(
  "testReg:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : testDb",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_config <- paste0(
  "reg:",
  "\n  smerte:",
  "\n    nationalAccess:",
  "\n      reshId : 10",
  "\n      userRole : SC",
  "\n      nameKey : Nasjonal",
  "\n    ousAccess:",
  "\n      reshId :",
  "\n      - 21",
  "\n      - 22",
  "\n      userRole : LC",
  "\n      nameKey : 20"
)
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))
writeLines(test_config, cf)
close(cf)

# make queries for creating tables
fc <- file(system.file("testDb.sql", package = "smerte"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

test_that("relevant test database and tables can be made", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testReg")$con
  for (i in seq_len(length(queries))) {
    expect_equal(class(RMariaDB::dbExecute(con, queries[i])), "integer")

  }
  rapbase::rapCloseDbConnection(con)
})

# onto main testing
test_that("hospital name can be read from db", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testReg")$con
  query <- paste("INSERT INTO avdelingsoversikt SET DEPARTMENT_ID=1,",
                 "DEPARTMENT_CENTREID = 1,",
                 "DEPARTMENT_ACTIVE = 1,",
                 "LOCATION_SHORTNAME='s1';")
  RMariaDB::dbExecute(con, query)
  query <- paste("INSERT INTO avdelingsoversikt SET DEPARTMENT_ID=2,",
                 "DEPARTMENT_CENTREID = 20,",
                 "DEPARTMENT_ACTIVE = 1,",
                 "LOCATION_SHORTNAME='s20';")
  RMariaDB::dbExecute(con, query)
  query <- paste("INSERT INTO avdelingsoversikt SET DEPARTMENT_ID=3,",
                 "DEPARTMENT_CENTREID = 21,",
                 "DEPARTMENT_ACTIVE = 1,",
                 "LOCATION_SHORTNAME='s21';")
  RMariaDB::dbExecute(con, query)
  query <- paste("INSERT INTO avdelingsoversikt SET DEPARTMENT_ID=4,",
                 "DEPARTMENT_CENTREID = 22,",
                 "DEPARTMENT_ACTIVE = 1,",
                 "LOCATION_SHORTNAME='s22';")
  RMariaDB::dbExecute(con, query)
  expect_equal(class(getHospitalName("testReg", 1, "SC")), "character")
  expect_equal(getHospitalName("testReg", 1), "s1")
  #expect_warning(getHospitalName("testReg", 2))
  rapbase::rapCloseDbConnection(con)
})

test_that("multiple hospital names can be returned", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testReg")$con
  expect_equal(class(getHospitalName("testReg", 20, "LC")), "character")
  expect_equal(getHospitalName("testReg", 21, "LC"), "s21 og s22")
  rapbase::rapCloseDbConnection(con)
})

test_that("name-id mapping can be obtained", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testReg")$con
  expect_equal(class(getNameReshId("testReg")), "data.frame")
  expect_equal(class(getNameReshId("testReg", asNamedList = TRUE)),
               "list")
  rapbase::rapCloseDbConnection(con)
})

test_that("tables can be dumped", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testReg")$con
  expect_equal(class(
    getDataDump("testReg", "allevar", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "allevarnum", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "avdelingsoversikt", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "forlopsoversikt", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "skjemaoversikt", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "SmerteDiagnoser", Sys.Date(), Sys.Date())
  ), "data.frame")
  expect_equal(class(
    getDataDump("testReg", "smertediagnosernum", Sys.Date(), Sys.Date())
  ), "data.frame")
  rapbase::rapCloseDbConnection(con)
})

test_that("data for lokal tilsyn can be queried", {
  check_db()
  expect_equal(class(getRegDataLokalTilsynsrapportMaaned(
    "testReg", 1, "SC", Sys.Date(), Sys.Date())),
    "data.frame"
  )
})

test_that("data for dekningsgrad can be queried", {
  check_db()
  expect_equal(class(getRegDataRapportDekningsgrad(
    "testReg", 1, "SC", Sys.Date(), Sys.Date())),
    "data.frame"
  )
})

test_that("data for indikator can be queried", {
  check_db()
  expect_equal(class(getRegDataIndikator(
    "testReg", 1, "SC", Sys.Date(), Sys.Date())),
    "data.frame"
  )
})

test_that("data for SmerteDiagKatValueLab can be queried", {
  check_db()
  expect_equal(class(getSmerteDiagKatValueLab(
    "testReg", 1)),
    "list"
  )
})

test_that("data for smertekategori can be queried", {
  check_db()
  expect_equal(class(getRegDataSmertekategori(
    "testReg", 1, "SC", Sys.Date(), Sys.Date())),
    "data.frame"
  )
})

test_that("data for spinalkateter can be queried", {
  check_db()
  expect_equal(class(getRegDataSpinalkateter(
    "testReg", 1, "SC", Sys.Date(), Sys.Date())),
    "data.frame"
  )
})

test_that("data for local years can be queried", {
  check_db()
  expect_equal(class(getLocalYears(
    "testReg", 1, "SC")),
    "data.frame"
  )
})

test_that("data for all years can be queried", {
  check_db()
  expect_equal(class(getAllYears(
    "testReg", 1, "SC")),
    "data.frame"
  )
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection("testReg")$con
  RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)
