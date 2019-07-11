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
#' ## POST the formula of Aspirin to get a query ID
#' formula <- "C9H8O4"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula(formula = formula, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_formula <- function(formula, dataSources, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.null(formula)) {
    stop("No \"formula\" provided.", call. = FALSE)
  }
  
  if (length(formula) > 1) {
    stop("This function can only handle individual \"formula\" entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (any(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount"))) {
    stop("No valid \"orderBy\" provided.", call. = FALSE)
  }
  
  if (any(tolower(orderDirection) %in% c("ascending", "descending"))) {
    stop("No valid \"orderDirection\" provided.", call. = FALSE)
  }
  
  if (length(apikey) > 1L) {
    stop("This function can only handle a single ChemSpider \"apikey\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (length(dataSources) == 1) {
    dataSources <- I(dataSources)
  }
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    } else {
      dataSources <- paste(dataSources, collapse = ",")
    }
    curl_data <- list("formula" = formula, "dataSources" = dataSources, "orderBy" = orderBy, "orderDirection" = orderDirection)
  } else {
    curl_data <- list("formula" = formula, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/formula"
  
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
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".\n", error_message)
    
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
