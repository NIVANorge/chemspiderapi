#' @title Get details for a ChemSpider record
#' @description This function returns record details from ChemSpider.
#' @details "Call this endpoint with a Record ID as an integer.\cr
#' \cr
#' The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."
#' @param recordId A valid (integer) ChemSpider ID.
#' @param fields Either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output? Defaults to \code{TRUE}.
#' @param simplify_formula \code{logical}: Should formula strings be simplified? Defaults to \code{FALSE}.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @return A \code{data.frame} if multiple columns are returned, or a vector of the appropriate type if only one \code{field} is returned.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Get the record details for caffeine
#' recordId <- 2424L
#' apikey <- "A valid 32-character Chemspider API key"
#' get_recordId_details(recordId = recordId, 
#'                      fields = c("SMILES", "Formula", "MolecularWeight", "CommonName"), 
#'                      apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_recordId_details <- function(recordId, 
                                 fields = "all", 
                                 apikey, 
                                 id = TRUE, 
                                 simplify_formula = FALSE,
                                 coerce = FALSE) {
  
  .check_recordId(recordId)

  .check_fields(fields)

  .check_apikey(apikey)
  
  .check_coerce(coerce)

  if (length(fields) == 1L) {
    if (fields == "all") {
      fields <- "SMILES,Formula,InChI,InChIKey,StdInChI,StdInChIKey,AverageMass,MolecularWeight,MonoisotopicMass,NominalMass,CommonName,ReferenceCount,DataSourceCount,PubMedCount,RSCCount,Mol2D,Mol3D"
    } else {
      fields <- fields
    }
  } else {
    fields <- paste(fields, collapse = ",")
  }

  header <- list("Content-Type" = "", "apikey" = apikey)

  base_url <- Sys.getenv("GET_RECORDID_URL",
                         "https://api.rsc.org/compounds/v1/records/")
  
  url <- paste0(base_url, recordId, "/details?fields=", fields)

  handle <- curl::new_handle()

  curl::handle_setopt(handle, customrequest = "GET")

  curl::handle_setheaders(handle, .list = header)

  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)

  .check_status_code(raw_result$status_code)

  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (!id) {
    result$id <- NULL
  }
  
  if (grepl("formula", fields, ignore.case = TRUE) && simplify_formula) {
    result$formula <- gsub(pattern = "[[:punct:]]", replacement = "", x = result$formula)
  }

  if (coerce) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  result
}
