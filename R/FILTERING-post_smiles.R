#' @title POST a SMILES string
#' @description Functionality to POST a SMILES string to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryID_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details If successful, returns the \code{queryId} as character string.\cr
#' @param smiles A SMILES character string.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/smiles}
#' @examples \dontrun{
#' ## POST the SMILES string of aspirin to get a queryId
#' smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_smiles(smiles = smiles, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_smiles <- function(smiles, apikey) {
  
  check_smiles(smiles)
  
  check_apikey(apikey)
  
  data <- list("smiles" = smiles)
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/smiles"
  
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
