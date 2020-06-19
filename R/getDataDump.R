#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate String with start period, endpoint included
#' @param toDate String with end period, endpont included
#' @param ... Additional parmeters to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr select left_join
#'
#' @return A data frame with registry data
#' @export
#'

getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {

  # Datadumper vi vil ha mulighet til å hente ned:
  if( tableName %in% c( "AlleVar"
                        , "AlleVarNum"
                        , "avdelingsoversikt"
                        , "ForlopsOversikt"
                        , "SmerteDiagnoser"
                        , "SmerteDiagnoserNum")
  ){
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  ProsedyreDato >= '", fromDate, "' AND ProsedyreDato <= '", toDate, "';"
    )
  }


  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("Smertereg data dump:\n", query))
  }


  # Henter tabellen som skal lastes ned av bruker:
  tab <- rapbase::LoadRegData(registryName, query)


  # Returnerer tabell
  return( tab )

}
