#' @title POST an intrinsic property
#' @description Functionality to POST an intrinsic property to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details Valid entries for property are: \code{formula}, \code{molecularWeight}, \code{nominalMass}, \code{averageMass}, or \code{monoisotopicMass}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"Valid values for \code{complexity} are \code{any}, \code{single}, or \code{multiple} whereby a compound with a complexity of multiple has more than one disconnected system in it or a metal atom or ion.\cr
#' \cr
#' Valid values for \code{isotopic} are \code{any}, \code{labeled}, or \code{unlabeled}.\cr
#' \cr
#' You can use \code{orderBy} and \code{orderDirection} to specify the sort order for the results. Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character vector.
#' @param property See Details.
#' @param formula See Details.
#' @param complexity See Details.
#' @param isotopic See Details.
#' @param mass See Details.
#' @param range See Details.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/intrinsicproperty}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST an intrinsic property for Aspirin to get a query ID
#' property = "Formula"
#' formula = "C9H8O4"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, formula = formula, apikey = apikey)
#' 
#' property <- "MolecularWeight"
#' mass <- 180
#' range <- 0.5
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_intrinsicproperty <- function(property, formula = NULL, complexity = "any", isotopic = "any", mass = NULL, range = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.null(property)) {
    stop("No \"property\" provided.", call. = FALSE)
  }
  
  if (length(property) > 1) {
    stop("This function is meant for single \"property\" entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!any(tolower(property) %in% c("formula", "molecularweight", "nominalmass", "averagemass", "monoisotopicmass"))) {
    stop("The provided property is not supported; please consult the help file for a list of available properties", call. = FALSE)
  }
  
  if (property != "formula" && is.null(mass) || property != "formula" && is.null(range)) {
    stop("Both \"mass\" and \"range\" need to be provided.", call. = FALSE)
  }
  
  if (property == "formula" && is.null(formula)) {
    stop("No \"formula\" provided.", call. = )
  }
  
  if (any(is.na(as.double(mass)))) {
    stop("The provided \"mass\" is not a valid (double) number.", call. = FALSE)
  }
  
  if (any(is.na(as.double(range)))) {
    stop("The provided \"range\" is not a valid (double) number.", call. = FALSE)
  }
  
  if (!is.null(mass) && mass < 1 || !is.null(mass) && mass > 11000) {
    stop("The provided \"mass\" is outside ChemSpider's settings [1,11000].", call. = FALSE)
  }
  
  if (!is.null(range) && range < 0.0001 || !is.null(range) && range > 100) {
    stop("The provided \"range\" is outside ChemSpider's settings [0.0001,100].", call. = FALSE)
  }
  
  if (length(mass) != length(range)) {
    stop("Every \"mass\" needs a \"range\", and vice verca. Please provide equal length vectors.", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (!is.null(complexity) && !any(tolower(complexity) %in% c("any", "single", "multiple"))) {
    stop("Please provide \"complexity\" as either \"any\", \"single\", or \"multiple\".")
  }
  
  if (!is.null(isotopic) && !any(tolower(isotopic) %in% c("any", "labeled", "unlabeled"))) {
    stop("Please provide \"isotopic\" as either \"any\", \"labeled\", or \"unlabeled\".")
  }
  
  options <- list("complexity" = complexity, "isotopic" = isotopic)
  
  if (tolower(property) == "formula") {
    curl_data <- list("formula" = formula, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "molecularweight") {
    molecularWeight <- list("mass" = mass, "range" = range)
    curl_data <- list("molecularWeight" = molecularWeight, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "nominalmass") {
    nominalMass <- list("mass" = mass, "range" = range)
    curl_data <- list("nominalMass" = nominalMass, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "averagemass") {
    averageMass <- list("mass" = mass, "range" = range)
    curl_data <- list("averageMass" = averageMass, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "monoisotopicmass") {
    monoisotopicMass <- list("mass" = mass, "range" = range)
    curl_data <- list("monoisotopicMass" = monoisotopicMass, "options" = options, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/intrinsicproperty"
  
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
