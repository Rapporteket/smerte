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
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             host = Sys.getenv("MYSQL_HOST"),
                             user = Sys.getenv("MYSQL_USER"),
                             password = Sys.getenv("MYSQL_PASSWORD"),
                             bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testDb;")
  RMariaDB::dbDisconnect(con)
}

# make temporary config
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())

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
  con <- rapbase::rapOpenDbConnection("testDb")$con
  for (i in seq_len(length(queries))) {
    expect_equal(class(RMariaDB::dbExecute(con, queries[i])), "integer")

  }
  rapbase::rapCloseDbConnection(con)
})

# onto main testing
test_that("veiledning report can be produced as file", {
  check_db()
  expect_true(file.exists(reportProcessor("veiledning", title = "Test")))
})

test_that("tilsyn report can be produced as file", {
  #check_db()
  testthat::skip("This report do not handle empty data. Please fix!")
  expect_true(file.exists(reportProcessor("tilsyn", title = "Test",
                                          registryName = "testDb")))
})

test_that("indikator report can be produced as file", {
  #check_db()
  testthat::skip("This report do not handle empty data. Please fix!")
  expect_true(file.exists(reportProcessor("indikator", title = "Test",
                                          registryName = "testDb")))
})

test_that("nasjonalIndikator report can be produced as file", {
  check_db()
  testthat::skip("This report does not run in ci env. Please fix!")
  expect_true(file.exists(reportProcessor("nasjonalIndikator", title = "Test",
                                          registryName = "testDb")))
})

test_that("spinalkateter report can be produced as file", {
  check_db()
  testthat::skip("This report does not run in ci env. Please fix!")
  expect_true(file.exists(reportProcessor("spinalkateter", title = "Test",
                                          registryName = "testDb")))
})

test_that("unknown report can be produced error", {
  check_db()
  expect_error(reportProcessor("noneExistingReport", title = "Test",
                               registryName = "testDb"))
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection("testDb")$con
  RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)
