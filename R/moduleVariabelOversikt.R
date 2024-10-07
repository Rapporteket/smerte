# variabeloversiktinput <- function(
#     id,
#     startDate = lubridate::today() - lubridate::years(1),
#     endDate = lubridate::today() - lubridate::weeks(1),
#     min = "1980-01-01",
#     max = "2100-01-01",
#     avdeling = shiny::selectInput(
#       inputid = avdeling,
#       label = ,
#       choices,
#     )) {
#
#   shiny::tagList(
#     shiny::dateRangeInput(shiny::NS(id, "dateRange"),
#                           label = "Velg periode:",
#                           start = startDate,
#                           end = endDate,
#                           min = min,
#                           max = max,
#                           separator = "-"),
#     shiny::radioButtons(shiny::NS(id, "format"),
#                         "Format for nedlasting",
#                         list(PDF = "pdf", HTML = "html"),
#                         inline = FALSE),
#     shiny::downloadButton(shiny::NS(id, "downloadReport"), "Last ned!")
#   )
#
# }


# reshId <- rapbase::getUserReshId(session)
# registryName <- smerte::makeRegistryName("smerte", reshId)
# userFullName <- rapbase::getUserFullName(session)
# userRole <- rapbase::getUserRole(session)
# hospitalName <- smerte::getHospitalName(registryName, reshId, userRole)
# author <- userFullName


#d_avd = getDataDump(registryName =  tableName = "avdelingsoversikt")


# avdavdelingsliste = function(){
#
# }
