#' POST elements to obtain a query ID
#' 
#' Functionality to POST up to 15 elements to include and up to 100 elements to exclude to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' Says Chemspider:\cr
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
#' Valid values for \code{isotopic} are \code{"any"}, \code{"labeled"}, or \code{"unlabeled"}."\cr
#' \cr
#' If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param includeElements A character vector of elements to include; maximum length 15.
#' @param excludeElements A character vector of elements to exclude; maximum length 100.
#' @param includeAll \code{logical}: Only look for records containing ALL elements of \code{includeElement}?
#' @param complexity See Details.
#' @param isotopic See Details.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the query ID as character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/element}
#' @examples 
#' \dontrun{
#' ## POST the elements for Aspirin and exclude certain elements
#' includeElements <- c("C", "H", "O")
#' excludeElements <- c("K", "Na", "N", "Cl", "F")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_element(includeElements = includeElements, excludeElements = excludeElements, apikey = apikey)
#' }
#' @export
post_element <- function(includeElements, excludeElements, includeAll = FALSE, complexity = "any", isotopic = "any", orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(includeElements)) {
    warning("No \"includeElements\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(includeElements) > 15) {
    warning("ChemSpider only supports up to 15 entries in \"includeElements\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(excludeElements) > 100) {
    warning("ChemSpider only supports up to 100 entries in \"excludeElements\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(complexity) %in% c("any", "single", "multiple")) != 1) {
    warning("The provided complexity is not \"any\", \"single\", or \"multiple\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(isotopic) %in% c("any", "labeled", "unlabeled")) != 1) {
    warning("The provided isotopic is not \"any\", \"labeled\", or \"unlabeled\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderBy) > 1) {
    warning("Only a single \"orderBy\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("Please provide a valid input for \"orderBy\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderDirection) > 1) {
    warning("Only a single \"orderDirection\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\"; returning NA.", call. = FALSE)
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
  
  if (length(includeElements) == 1) {
    includeElements <- I(includeElements)
  }
  
  if (length(excludeElements) == 1) {
    excludeElements <- I(excludeElements)
  }
  
  options <- list(includeAll = includeAll, complexity = complexity, isotopic = isotopic)
  
  curlData <- list(includeElements = includeElements, excludeElements = excludeElements, options = options, orderBy = orderBy, orderDirection = orderDirection)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/element"
  
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
