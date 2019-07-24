#' @title Get the status of a ChemSpider query
#' @description This function is used to return the status of a query from ChemSpider before \code{chemspiderapi::get_results()} is called.
#' @details Call this endpoint with a \code{queryId} obtained from a previous POST query,e.g., \code{chemspiderapi::post_inchikey()}.
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
#' @return A character vector indicating the status of the query
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/status}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## GET the status of a query from ChemSpider
#' queryId <- "A valid 36-character Chemspider query ID"
#' apikey <- "A valid 32-character Chemspider API key"
#' get_queryId_status(queryId = queryId, apikey = apikey)
#' }
#' @export
get_queryId_status <- function(queryId, count = TRUE, message = TRUE, apikey) {
  
  check_queryId(queryId)
  
  check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/status")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  if (isFALSE(count)) {
    result$count <- NULL
  }
  
  if (isFALSE(message)) {
    result$message <- NULL
  }
  
  check_result(result)
  
  result
}
