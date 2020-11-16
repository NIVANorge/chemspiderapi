#' @title Post a name to obtain a ChemSpider query ID
#' @description Functionality to POST the name of a compound and obtain a ChemSpider query ID for subsequent use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Allowed entries for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Allowed entries for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.
#' @param name A character string of the compound name.
#' @param orderBy A character string indicating by which parameter the results should be arranged (NOT case sensitive); see Details.
#' @param orderDirection A character string indicating which in which direction the results should be arranged (NOT case sensitive); see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @param simplify \code{logical}: should the results be simplified to a vector? Defaults to \code{FALSE}.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/name}
#' @examples \dontrun{
#' ## Post the name of caffeine to obtain a query ID
#' name <- "caffeine"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_name(name = name, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_name <- function(name, 
                      orderBy = "recordId", 
                      orderDirection = "ascending", 
                      apikey,
                      coerce = FALSE, simplify = FALSE) {
  
  .check_name(name)
  
  .check_order(orderBy, orderDirection)
  
  .check_apikey(apikey)
  
  .check_coerce(coerce)
  
  .check_simplify(simplify)
  
  data <- list("name" = name, 
               "orderBy" = orderBy, 
               "orderDirection" = orderDirection)
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/name"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (coerce) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  if (simplify) {
    result <- unlist(result, use.names = FALSE)
  }
  
  result
}
