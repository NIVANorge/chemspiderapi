#' GET external references of a record ID
#' 
#' GET a list of external references for a ChemSpider ID.
#' 
#' It is recommended to specify which \code{dataSources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspiderapi::get_datasources()} for a complete list of the >300 \code{dataSources}.\cr
#' \cr
#' If successful, returns a data frame with four columns: \code{source}, \code{sourceUrl}, \code{externalId}, and \code{externalUrl}; unless any of them are removed by setting them to \code{FALSE} in the function call. In case only one parameter, e.g. the external ID, is returned, the result is a vector.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param recordId A valid (integer) ChemSpider ID.
#' @param dataSources Either a character string, a vector of characters, or a list of characters detailing which \code{dataSources} to look up. If left as is, will return all \code{dataSources}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param source \code{logical}: Should the source name be returned (ChemSpider default)?
#' @param sourceUrl \code{logical}: Should the source URL be returned (ChemSpider default)?
#' @param externalUrl \code{logical}: Should the external URL be returned (ChemSpider default)?
#' @return Either a data frame (if no options are dropped) or a character/integer vector, depending on the external ID.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/externalreferences}
#' @examples 
#' \dontrun{
#' ## GET the PubChem external reference for aspirin
#' recordId <- 2157L
#' dataSources <- "PubChem"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_externalreferences(recordId = recordId, dataSources = dataSources, apikey = apikey)
#' }
#' @export 
get_recordId_externalreferences <- function(recordId, dataSources, apikey, source = TRUE, sourceUrl = TRUE, externalUrl = TRUE) {
  
  check_recordId(recordId)
  
  check_dataSources(dataSources)
  
  check_apikey(apikey)
  
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    } else {
      dataSources <- paste(dataSources, collapse = ",")
    }
    url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences?dataSources=", dataSources)
  } else {
    url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences")
  }

  header <- list("Content-Type" = "", "apikey" = apikey)
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  if (isFALSE(source)) {
    result$source <- NULL
  }
  
  if (isFALSE(sourceUrl)) {
    result$sourceUrl <- NULL
  }
  
  if (isFALSE(externalUrl)) {
    result$externalUrl <- NULL
  }
  
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  
  result
}
