#' @title Get the status of a ChemSpider query
#' @description This function is used to return the status of a query from ChemSpider before \code{get_results()} is called.
#' @details Call this endpoint with a \code{queryId} obtained from a previous POST query,e.g., \code{post_inchikey()}.
#' \cr
#' If the query is still ongoing, returns a warning and a character vector of the query status as \code{Incomplete}. It is recommended to wait at least ten seconds before checking the status again.\cr
#' \cr
#' If the query is finalized, returns a data frame of the query status with \code{status}, \code{count} and \code{message}. The \code{status} can be either \code{Complete}, \code{Suspended}, \code{Failed}, or \code{Not Found}.\cr
#' \cr
#' \emph{A status of Suspended can be caused if the results could not be compiled within a reasonable amount of time. Create a new filter request with more restrictive parameters.}\cr
#' \cr
#' \emph{A status of Failed can be caused if the backend system could not compile the results. Create a new filter request and, if the same outcome occurs, apply more restrictive parameters.}\cr
#' \cr
#' \emph{A status of Not Found can be caused if the Query ID has not been registered or has expired. Create a new filter request.}
#' @param queryId A valid 36-character ChemSpider query ID string; see Details.
#' @param count \code{logical}: Should the count of the results be returned?
#' @param message \code{logical}: Should the message be returned?
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a \code{data.frame}? Defaults to \code{FALSE}.
#' @param simplify \code{logical}: should the results be simplified to a \code{vector}? Defaults to \code{FALSE}.
#' @return Returns the query status as \code{list}, \code{data.frame} or character \code{vector}.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/status}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## GET the status of a query from ChemSpider
#' queryId <- "A valid 36-character Chemspider query ID"
#' apikey <- "A valid 32-character Chemspider API key"
#' get_queryId_status(queryId = queryId, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_queryId_status <- function(queryId, count = TRUE, message = TRUE, apikey, coerce = FALSE, simplify = FALSE) {
  
  .check_queryId(queryId)
  .check_apikey(apikey)
  .check_coerce(coerce)
  .check_simplify(simplify)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/status")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (!count) {
    result$count <- NULL
  }
  
  if (!message) {
    result$message <- NULL
  }
  
  if (coerce) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  if (simplify && !count && !message) {
    result <- unlist(result, use.names = FALSE)
  }
  
  if (simplify && (count || message)) {
    warning("Simplification not possible, \"count\" and/or \"warning\" are set to TRUE.", 
            call. = FALSE)
  }
  
  result
}
