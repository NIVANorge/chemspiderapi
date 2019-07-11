#' @title POST elements to obtain a query ID
#' @description Functionality to POST up to 15 elements to include and up to 100 elements to exclude to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
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
#' Valid values for \code{isotopic} are \code{"any"}, \code{"labeled"}, or \code{"unlabeled"}."\cr
#' \cr
#' If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
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
#' ## POST the elements for Aspirin and exclude certain elements
#' includeElements <- c("C", "H", "O")
#' excludeElements <- c("K", "Na", "N", "Cl", "F")
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_element(includeElements = includeElements, excludeElements = excludeElements, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_element <- function(includeElements, excludeElements, includeAll = FALSE, complexity = "any", isotopic = "any", orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.null(includeElements)) {
    stop("No \"includeElements\" provided.", call. = FALSE)
  }
  
  if (length(includeElements) > 15) {
    stop("ChemSpider only supports up to 15 entries in \"includeElements\".", call. = FALSE)
  }
  
  if (length(excludeElements) > 100) {
    stop("ChemSpider only supports up to 100 entries in \"excludeElements\".", call. = FALSE)
  }
  
  if (!any(tolower(complexity) %in% c("any", "single", "multiple"))) {
    stop("The provided complexity is not \"any\", \"single\", or \"multiple\".", call. = FALSE)
  }
  
  if (!any(tolower(isotopic) %in% c("any", "labeled", "unlabeled"))) {
    stop("The provided isotopic is not \"any\", \"labeled\", or \"unlabeled\".", call. = FALSE)
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
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (length(includeElements) == 1) {
    includeElements <- I(includeElements)
  }
  
  if (length(excludeElements) == 1) {
    excludeElements <- I(excludeElements)
  }
  
  options <- list("includeAll" = includeAll, "complexity" = complexity, "isotopic" = isotopic)
  
  curl_data <- list("includeElements" = includeElements, "excludeElements" = excludeElements, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/element"
  
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
