#' @title POST a chemical formula to obtain a query ID
#' @description Functionality to POST a formula to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.
#' @param formula A character string of a chemical formula.
#' @param dataSources Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST the formula of caffeine to get a query ID
#' formula <- "C8H10N4O2"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula(formula = formula, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_formula <- function(formula, 
                         dataSources = NULL, 
                         orderBy = "recordId", 
                         orderDirection = "ascending", 
                         apikey) {
  
  check_formula(formula)
  
  check_order(orderBy, orderDirection)
  
  check_apikey(apikey)
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1L) {
      dataSources <- I(dataSources)
    }
    data <- list("formula" = formula, 
                 "dataSources" = dataSources, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  } else {
    data <- list("formula" = formula, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/formula"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  check_result(result)
  
  result
}
