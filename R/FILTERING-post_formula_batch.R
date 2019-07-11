#' @title POST a batch of chemical formulas
#' @description Functionality to POST a batch of up to 100 formulas to obtain a \code{queryId} for use in \code{chemspiderapi::get_formula_batch_queryId_status()} and \code{chemspiderapi::get_formula_batch_queryId_results()}.
#' @details Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.
#' @param formulas A character vector of up to 100 chemical formulas.
#' @param dataSources Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST the formulas of aspirin and caffeine to get a query ID
#' formulas <- c("C9H8O4", "C8H10N4O2")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula_batch(formulas = formulas, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_formula_batch <- function(formulas, dataSources, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.null(formulas)) {
    stop("No \"formulas\" provided.", call. = FALSE)
  }
  
  if (length(formulas) == 1) {
    stop("This is function is meant for multiple \"formulas\".\nFor an individual \"formula\" approach, try chemspiderapi::post_formula().", call. = FALSE)
  }
  
  if (length(formulas) > 100) {
    stop("This is only meant for up to 100 \"formula\".", call. = FALSE)
  }
  
  if (any(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount"))) {
    stop("No valid \"orderBy\" provided.", call. = FALSE)
  }
  
  if (any(tolower(orderDirection) %in% c("ascending", "descending"))) {
    stop("No valid \"orderDirection\" provided.", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    } else {
      dataSources <- paste(dataSources, collapse = ",")
    }
    curl_data <- list("formulas" = formulas, "dataSources" = dataSources, "orderBy" = orderBy, "orderDirection" = orderDirection)
  } else {
    curl_data <- list("formulas" = formulas, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/formula/batch"
  
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
    
    if (raw_result$status_code == 429L) {
      error_message <- "ChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (raw_result$status_code == 500L) {
      error_message <- "ChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (raw_result$status_code == 503L) {
      error_message <- "ChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained.\nCarefully check the \"inchikey\" and the validity of the \"apikey\".\n", error_message)
    
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
