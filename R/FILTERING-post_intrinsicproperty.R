#' @title Post an intrinsic property
#' @description Functionality to post an intrinsic property to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
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
#' ## Post an intrinsic property for caffeine to get a query ID
#' property = "Formula"
#' formula = "C8H10N4O2"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, formula = formula, apikey = apikey)
#' 
#' property <- "MolecularWeight"
#' mass <- 194
#' range <- 0.5
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_intrinsicproperty <- function(property, 
                                   formula = NULL, 
                                   complexity = "any", 
                                   isotopic = "any", 
                                   mass = NULL, 
                                   range = NULL, 
                                   orderBy = "recordId", 
                                   orderDirection = "ascending", 
                                   apikey) {
  
  .check_property(property)
  
  if (property != "formula" && is.null(mass) || property != "formula" && is.null(range)) {
    stop("Both \"mass\" and \"range\" need to be provided.", 
         call. = FALSE)
  }
  
  if (property == "formula" && is.null(formula)) {
    stop("No \"formula\" provided.", 
         call. = FALSE)
  }
  
  .check_mass_and_range(mass, range)
  
  .check_apikey(apikey)
  
  .check_complexity(complexity)
  
  .check_isotopic(isotopic)
  
  .check_order(orderBy, orderDirection)
  
  options <- list("complexity" = complexity, "isotopic" = isotopic)
  
  if (tolower(property) == "formula") {
    data <- list("formula" = formula, 
                 "options" = options, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "molecularweight") {
    molecularWeight <- list("mass" = mass, "range" = range)
    data <- list("molecularWeight" = molecularWeight, 
                 "options" = options, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "nominalmass") {
    nominalMass <- list("mass" = mass, "range" = range)
    data <- list("nominalMass" = nominalMass, 
                 "options" = options, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "averagemass") {
    averageMass <- list("mass" = mass, "range" = range)
    data <- list("averageMass" = averageMass, 
                 "options" = options, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  if (tolower(property) == "monoisotopicmass") {
    monoisotopicMass <- list("mass" = mass, "range" = range)
    data <- list("monoisotopicMass" = monoisotopicMass, 
                 "options" = options, 
                 "orderBy" = orderBy, 
                 "orderDirection" = orderDirection)
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/intrinsicproperty"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  .check_result(result)
  
  result
}
