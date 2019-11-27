#' Get hospital name from registry data
#'
#' Based on the hospital id (reshID) this function will return the name of
#' the corresponding hospital as provided in the registry data
#'
#' @param reshID string defining the resh ID
#'
#' @return string of hospital name
#' @export
#'
#' @examples
#' \dontrun{
#' getHospitalName("123456")
#' }

getHospitalName <- function(reshID) {

  regName <- paste0("smerte", reshID)
  dbType <- "mysql"
  query <- paste0("SELECT SykehusNavn FROM AlleVarNum WHERE AvdRESH = '",
                 reshID, "' LIMIT 1")

  d <- rapbase::LoadRegData(regName, dbType = dbType, query = query)[1,1]

  if (is.na(d)) {
    return("Ikke Eksisternede Sykehus")
  } else {
    return(d)
  }

}
