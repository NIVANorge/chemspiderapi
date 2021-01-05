#' @title Get results of a formula batch query from ChemSpider
#' @description Get results of a formula batch query from ChemSpider after \code{get_formula_batch_queryId_status()} returns \code{"Complete"}.
#' @details Before running \code{get_formula_batch_queryId_results()}, make sure \code{get_formula_batch_queryId_status()} returns \code{"Complete"}.
#' @param queryId A valid 36-character query ID, as returned by \code{post_formula_batch()}.
#' @param status A character string indicating the query status as returned by \code{get_formula_batch_queryId_status()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a \code{data.frame}? Defaults to \code{FALSE}.
#' @return Returns the (integer) ChemSpider IDs as \code{list} or \code{data.frame}.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/formula/batch/{queryId}/results}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples 
#' \dontrun{
#' ## Obtain the result from a mass batch query
#' apikey <- "a valid 32-character ChemSpider apikey"
#' queryId <- "a valid 36-character ChemSpider queryId"
#' status <- get_formula_batch_queryId_status(queryId = queryId, apikey = apikey)
#' get_formula_batch_queryId_results(queryId = queryId, status = status, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_formula_batch_queryId_results <- function(queryId, status, apikey, coerce = FALSE) {
  
  .check_queryId(queryId)
  .check_status(status)
  .check_apikey(apikey)
  .check_coerce(coerce)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  base_url <- Sys.getenv("GET_FORMULA_BATCH_QUERYID_URL", 
                         "https://api.rsc.org/compounds/v1/filter/formula/batch/")
  
  url <- paste0(base_url, queryId, "/results")
  
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "GET")
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (coerce) {
    result <- result$batchResults
  }
  
  result
}
