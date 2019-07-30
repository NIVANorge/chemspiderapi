#' @title Get external references of a record ID
#' @description GET a list of external references for a ChemSpider ID.
#' @details It is recommended to specify which \code{dataSources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspiderapi::get_datasources()} for a complete list of the >350 \code{dataSources}.
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
#' ## Get the PubChem external reference for caffeine
#' recordId <- 2424L
#' dataSources <- "PubChem"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_externalreferences(recordId = recordId, dataSources = dataSources, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export 
get_recordId_externalreferences <- function(recordId, dataSources = NULL, apikey, source = TRUE, sourceUrl = TRUE, externalUrl = TRUE) {
  
  check_recordId(recordId)

  check_dataSources(dataSources)
  
  check_apikey(apikey)

  if (!is.null(dataSources)) {
    if (length(dataSources) > 1) {
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
  
  check_result(result)
  
  result
}
