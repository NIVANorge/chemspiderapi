#' @title Post a monoisotopic mass and its range to obtain a query ID
#' @description Functionality to POST an atomic mass and its range to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details \emph{"Submit mass as a double between 1 and 11000 Atomic Mass Units, and a range between 0.0001 and 100 via POST. For example, if you specify a mass of 40 and a range of 5, all monoisotopic masses between 35 and 45 are searched.\cr
#' \cr
#' [...] If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}
#' @param mass A (double) number corresponding to the atomic mass (Da or g/mol) you are inquiring. Has to be within the range of [1,11000].
#' @param range The range for the above mass, also as (double) number. Has to be within the range of [0.0001,100].
#' @param dataSources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Post the approximate atomic mass of caffeine and a sensible range
#' mass <- 194
#' range <- 0.002
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass(mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_mass <- function(mass, range, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  check_mass_and_range(mass, range)
  
  check_dataSources(dataSources)
  
  check_order(orderBy, orderDirection)
  
  check_apikey(apikey)
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    } 
    data <- list("mass" = mass, "range" = range, "dataSources" = dataSources, "orderBy" = orderBy, "orderDirection" = orderDirection)
  } else {
    data <- list("mass" = mass, "range" = range, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/mass"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  check_result(result)
  
  result
}
