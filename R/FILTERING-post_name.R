#' @title POST a name to obtain a ChemSpider query ID
#' @description Functionality to POST the name of a compound and obtain a ChemSpider query ID for subsequent use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Allowed entries for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Allowed entries for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' If successful, returns a 36-character \code{queryId} as a character vector. The \code{queryId} can be used in \code{chemspiderapi::get_queryId_status()} and \code{chemspider::get_queryId_results()}.
#' @param name A character string of the compound name.
#' @param orderBy A character string indicating by which parameter the results should be arranged (NOT case sensitive); see Details.
#' @param orderDirection A character string indicating which in which direction the results should be arranged (NOT case sensitive); see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/name}
#' @examples \dontrun{
#' ## POST the name of aspirin to obtain a query ID
#' name <- "aspirin"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_name(name = name, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_name <- function(name, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.null(name)) {
    stop("No \"name\" provided.", call. = FALSE)
  }
  
  if (length(name) > 1) {
    stop("This function can only handle a single \"name\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (length(orderBy) > 1) {
    stop("Only a single \"orderBy\" entry is supported.", call. = FALSE)
  }
  
  if (!any(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount"))) {
    stop("Please provide a valid input for \"orderBy\".", call. = FALSE)
  }
  
  if (length(orderDirection) > 1) {
    stop("Only a single \"orderDirection\" entry is supported.", call. = FALSE)
  }
  
  if (!any(tolower(orderDirection) %in% c("ascending", "descending"))) {
    stop("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\".", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  curl_data <- list("name" = name, "orderBy" = orderBy, "orderDirection" = orderDirection)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/name"
  
  curl_handle <- curl::new_handle()
  
  curl::handle_setopt(curl_handle, customrequest = "POST", postfields = curl_data)
  
  curl::handle_setheaders(curl_handle, .list = curl_header)
  
  raw_result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)
  
  if (raw_result$status_code != 200L) {
    
    error_message <- "No ChemSpider Error Details were provided."
    
    if (raw_result$status_code == 400L) {
      error_message <- "ChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (raw_result$status_code == 401L) {
      error_message <- "ChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (raw_result$status_code == 404L) {
      error_message <- "ChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 405L) {
      error_message <- "ChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 413L) {
      error_message <- "ChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 429L) {
      error_message <- "ChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (raw_result$status_code == 500L) {
      error_message <- "ChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (raw_result$status_code == 503L) {
      error_message <- "ChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained.\nCarefully check the \"name\" and the validity of the \"apikey\".\n", error_message)
    
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
