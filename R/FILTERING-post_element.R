#' @title Post elements to obtain a query ID
#' @description Functionality to post up to 15 elements to include and up to 100 elements to exclude to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Says Chemspider:\cr
#' \cr
#' \emph{"Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default."}\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}.\cr
#' \cr
#' The default behavior is to consider records which contain any of the elements listed in \code{includeElements}. Setting \code{includeAll} to \code{TRUE} will only consider records which contain all the elements listed in the \code{includeElements}.\cr
#' \cr
#' Valid values for \code{complexity} are \code{"any"}, \code{"single"}, or \code{"multiple"} whereby a compound with a complexity of multiple has more than one disconnected system in it or a metal atom or ion.\cr
#' \cr
#' Valid values for \code{isotopic} are \code{"any"}, \code{"labeled"}, or \code{"unlabeled"}."
#' @param includeElements A character vector of elements to include; maximum length 15.
#' @param excludeElements A character vector of elements to exclude; maximum length 100.
#' @param includeAll \code{logical}: Only look for records containing ALL elements of \code{includeElement}?
#' @param complexity See Details.
#' @param isotopic See Details.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/element}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post the elements for caffeine and exclude certain elements
#' includeElements <- c("C", "H", "N", "O")
#' excludeElements <- c("K", "Na", "Fe", "Cl", "F")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_element(includeElements = includeElements, excludeElements = excludeElements, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_element <- function(includeElements, excludeElements, includeAll = FALSE, complexity = "any", isotopic = "any", orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  check_elements(includeElements, excludeElements)
  
  check_complexity(complexity)
  
  check_isotopic(isotopic)
  
  check_order(orderBy, orderDirection)

  check_apikey(apikey)
  
  if (length(includeElements) == 1) {
    includeElements <- I(includeElements)
  }
  
  if (length(excludeElements) == 1) {
    excludeElements <- I(excludeElements)
  }
  
  options <- list("includeAll" = includeAll, "complexity" = complexity, "isotopic" = isotopic)
  
  data <- list("includeElements" = includeElements, "excludeElements" = excludeElements, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/element"
  
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
