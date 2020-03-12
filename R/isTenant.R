#' Do the current id belong to a given organization?
#'
#' By matching organization ID and configuration, TRUE or FALSE is returned
#'
#' @param reshId Integer organization ID
#'
#' @return Logical (TRUE or FALSE)
#' @name isTenant
#' @aliases isNationalReg isOUSReg
NULL


#' @rdname isTenant
#' @export
isNationalReg <- function(reshId) {

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId == conf$reg$smerte$nationalAccess$reshId) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @rdname isTenant
#' @export
isOUSReg <- function(reshId) {

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId %in% conf$reg$smerte$ousAccess$reshId) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
