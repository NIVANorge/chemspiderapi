#' GET the results of a formula batch query from ChemSpider
#' 
#' GET the results of a formula batch query from ChemSpider after \code{chemspiderapi::get_formula_batch_queryId_status()} returns \code{"Complete"}.
#' 
#' Before running \code{chemspiderapi::get_formula_batch_queryId_results()}, make sure \code{chemspiderapi::get_formula_batch_queryId_status()} returns \code{"Complete"}!\cr
#' \cr
#' If successful, returns a data frame with the query results; in case the response is a single value, e.g., a ChemSpider ID, it is returned as single vector.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_int()}.
#' 
#' @param queryId A valid 36-character query ID, as returned by \code{chemspiderapi::post_formula_batch()}.
#' @param status status A character string indicating the query status as returned by \code{chemspiderapi::get_formula_batch_queryId_status()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the (integer) ChemSpider IDs
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/formula/batch/{queryId}/results}
#' @examples 
#' \dontrun{
#' ## Obtain the result from a mass batch query
#' apikey <- "a valid 32-character ChemSpider apikey"
#' queryId <- "a valid 36-character ChemSpider queryId"
#' status <- get_formula_batch_queryId_status(queryId = queryId, apikey = apikey)
#' get_formula_batch_queryId_results(queryId = queryId, status = status, apikey = apikey)
#' }
#' @export
get_formula_batch_queryId_results <- function(queryId, status, apikey) {
  
  if (is.na(queryId)) {
    warning("No valid \"queryId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (length(queryId) > 1L) {
    warning("This function can only handle a single ChemSpider \"queryId\" entry; returning \"NA\".\nMaybe you are looking for chemspideR::get_formula_batch_status() or chemspideR::get_mass_batch_status()?\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_integer_)
  }
  
  if (typeof(queryId) != "character") {
    warning("The ChemSpider \"queryId\" should be a 36-character string; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    warning("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    warning("The first part of the ChemSpider \"queryId\" should be 8 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    warning("The second part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    warning("The third part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    warning("The fourth part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    warning("The fifth part of the ChemSpider \"queryId\" should be 12 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (is.null(status)) {
    warning("No ChemSpider query \"status\" provided; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (status != "Complete") {
    warning("Query computation not yet completet; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_integer_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_integer_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_integer_)
  }
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/formula/batch/", queryId, "/results")
  
  curlHandle <- curl::new_handle()
  
  curl::handle_setopt(curlHandle, customrequest = "GET")
  
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  
  if (result$status_code != 200L) {
    
    error_message <- "\nNo ChemSpider Error Details were provided."
    
    if (result$status_code == 400L) {
      error_message <- "\nChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (result$status_code == 401L) {
      error_message <- "\nChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (result$status_code == 404L) {
      error_message <- "\nChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (result$status_code == 405L) {
      error_message <- "\nChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (result$status_code == 429L) {
      error_message <- "\nChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (result$status_code == 500L) {
      error_message <- "\nChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (result$status_code == 503L) {
      error_message <- "\nChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", error_message)
    
    warning(message, call. = FALSE)
    return(NA_integer_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(results = result$results, stringsAsFactors = FALSE)
  
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  
  return(result)
}
