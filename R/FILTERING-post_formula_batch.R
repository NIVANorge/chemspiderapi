#' POST a batch of chemical formulas
#' 
#' Functionality to POST a batch of up to 100 formulas to obtain a \code{queryId} for use in \code{chemspiderapi::get_formula_batch_queryId_status()} and \code{chemspiderapi::get_formula_batch_queryId_results()}.
#' 
#' Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param formulas A character vector of up to 100 chemical formulas.
#' @param dataSources Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a query ID as character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula/batch}
#' @examples 
#' \dontrun{
#' ## POST the formulas of aspirin and caffeine to get a query ID
#' formulas <- c("C9H8O4", "C8H10N4O2")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula_batch(formulas = formulas, apikey = apikey)
#' }
#' @export
post_formula_batch <- function(formulas, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(formulas)) {
    warning("No \"formulas\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(formulas) == 1) {
    warning("This is function is meant for multiple \"formulas\"; returning \"NA\".\nFor an individual \"formula\" approach, try chemspideR::post_formula().", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(formulas) > 100) {
    warning("This is only meant for up to 100 \"formula\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("No valid \"orderBy\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("No valid \"orderDirection\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(dataSources) == 1) {
    dataSources <- I(dataSources)
  }
  
  dataSources <- paste(dataSources, collapse = ",")
  
  curlData <- list(formulas = formulas, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/formula/batch"
  
  curlHandle <- curl::new_handle()
  
  curl::handle_setopt(curlHandle, customrequest = "POST", postfields = curlData)
  
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
    return(NA_character_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  return(result)
}
