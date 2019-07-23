#' @title POST a batch of monoisotopic masses and their ranges to obtain a query ID
#' @description Functionality to POST up to 20 monoisotopic masses and their range to obtain a \code{queryId} for use in \code{chemspiderapi::get_mass_batch_queryId_status()} and \code{chemspiderapi::get_mass_batch_queryId_results()}.
#' @details Says Chemspider:\cr
#' \cr
#' \emph{"If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by \code{recordId} by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}\cr
#' \cr
#' If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspider::get_mass_batch_queryId_status()} and \code{chemspideR::get_mass_batch_queryId_results()}.
#' @param mass A vector of (double) numbers corresponding to the atomic (monoisotopic) masses you are inquiring. Has to be within the range of [1,11000].
#' @param range A vector of (double) numbers corresponding to the rangees for the above masses. Has to be within the range of [0.0001,0.001].
#' @param dataSources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST two atomic (monoisotopic) masses and their ranges
#' mass <- c(180, 246)
#' range <- c(0.0005, 0.001)
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass_batch(mass = mass, range = range, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_mass_batch <- function(mass, range, dataSources, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  check_mass_and_range(mass, range)
  
  check_dataSources(dataSources)
  
  check_order(orderBy, orderDirection)
  
  check_apikey(apikey)
  
  masses <- list()
  
  for (i in 1:length(mass)) {
    masses[[i]] <- list("mass" = mass[i], "range" = range[i])
  }
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    } else {
      dataSources <- paste(dataSources, collapse = ",")
    }
    data <- list("masses" = masses, "dataSources" = dataSources, "orderBy" = orderBy, "orderDirection" = orderDirection)
  } else {
    data <- list("masses" = masses, "orderBy" = orderBy, "orderDirection" = orderDirection)
  }
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/mass/batch"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
