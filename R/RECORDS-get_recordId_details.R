#' @title GET the record details for a compound from ChemSpider
#' @description This function is used to return record details from ChemSpider.
#' @details The available options for \code{fields} are: \code{"SMILES"}, \code{"Formula"}, \code{"AverageMass"}, \code{"MolecularWeight"}, \code{"MonoisotopicMass"}, \code{"NominalMass"}, \code{"CommonName"}, \code{"ReferenceCount"}, \code{"DataSourceCount"}, \code{"PubMedCount"}, \code{"RSCCount"}, \code{"Mol2D"}, \code{"Mol3D"}, and \code{"all"}.\cr
#' \cr
#' If successful, returns either a \code{data.frame} of results from the query with the requested fields; if only one paraemeter is returned, this is then transformed into a vector.
#' @param recordId A valid (integer) ChemSpider ID.
#' @param fields Either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output?
#' @return A \code{data.frame} if multiple columns are returned, or a (named) vector of the appropriate type if only one \code{field} is returned.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## GET the record details for aspirin
#' recordId <- 2157L
#' apikey <- "A valid 32-character Chemspider API key"
#' get_recordId_details(recordId = recordId, 
#'                      fields = c("SMILES", "Formula", "MolecularWeight", "CommonName"), 
#'                      apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
get_recordId_details <- function(recordId, fields = "all", apikey, id = TRUE) {
  
  check_recordId(recordId)

  check_fields(fields)

  check_apikey(apikey)

  if (length(fields) == 1L) {
    if (fields == "all") {
      fields <- "SMILES,Formula,AverageMass,MolecularWeight,MonoisotopicMass,NominalMass,CommonName,ReferenceCount,DataSourceCount,PubMedCount,RSCCount,Mol2D,Mol3D"
    } else {
      fields <- fields
    }
  } else {
    fields <- paste(fields, collapse = ",")
  }

  header <- list("Content-Type" = "", "apikey" = apikey)

  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/details?fields=", fields)

  handle <- curl::new_handle()

  curl::handle_setopt(handle, customrequest = "GET")

  curl::handle_setheaders(handle, .list = header)

  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)

  check_status_code(raw_result$status_code)

  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  if (isFALSE(id)) {
    result$id <- NULL
  }

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  result
}
