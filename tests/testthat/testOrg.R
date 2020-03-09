# Test relevant for resolving org ids, org names and user roles

# Store current instance and prepare
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")

# Prepare config as yaml
tf <- tempfile()
td <- dirname(tf)
Sys.setenv(R_RAP_CONFIG_PATH=td)
Sys.setenv(R_RAP_INSTANCE="DEV")
conf <- list()
conf$reg$smerte$nationalAccess$reshId = 1
conf$reg$smerte$nationalAccess$userRole = "King"
conf$reg$smerte$nationalAccess$nameKey = "Global"
conf$reg$smerte$ousAccess$reshId = c(2, 3, 4)
conf$reg$smerte$ousAccess$userRole = "Earl"
conf$reg$smerte$ousAccess$nameKey = "Realm"

yaml::write_yaml(conf, tf)
file.rename(tf, paste(td, "rapbaseConfig.yml", sep = "/"))

test_that("the registry name can be provided based on config", {
  expect_equal(makeRegistryName("smerte", 1), "smerteGlobal")
  expect_equal(makeRegistryName("smerte", 3), "smerteRealm")
})

test_that("an arbitary registry name can be provided", {
  expect_equal(makeRegistryName("smerte", 10), "smerte10")
})



# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH=currentConfig)
Sys.setenv(R_RAP_INSTANCE=currentInstance)
