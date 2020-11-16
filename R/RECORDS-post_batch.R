#' @title Post a batch of ChemSpider record IDs
#' @description Post a batch query of up to 100 ChemSpider record IDs.
#' @details "The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."\cr
#' \cr
#' If a \code{recordId} is not found, it is silently dropped.
#' @param recordIds A vector of integer ChemSpider IDs.
#' @param fields A character string indicating which fields to return; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output? Defaults to \code{TRUE}.
#' @param simplify_formula \code{logical}: Should formula strings be simplified? Defaults to \code{FALSE}.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @return A data frame (if multiple fields are returned), or a vector of adequate type if only one field is required.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/records/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Post a query for the first 50 ChemSpider entries.
#' recordIds <- 1:50
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_batch(recordIds = recordIds, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_batch <- function(recordIds, 
                       fields = "all", 
                       apikey, 
                       id = TRUE, 
                       simplify_formula = FALSE,
                       coerce = FALSE) {
  
  .check_recordIds(recordIds)
  
  .check_fields(fields)
  
  .check_apikey(apikey)
  
  .check_coerce(coerce)
  
  if (length(fields) == 1L) {
    if (fields == "all") {
      fields <- c("SMILES","Formula","InChI","InChIKey","StdInChI","StdInChIKey","AverageMass","MolecularWeight","MonoisotopicMass","NominalMass","CommonName","ReferenceCount","DataSourceCount","PubMedCount","RSCCount","Mol2D","Mol3D")
    } else {
      fields <- fields
    }
  } else {
    fields <- I(fields)
  }
  
  data <- list("recordIds" = recordIds, "fields" = fields)
  
  data <- jsonlite::toJSON(data)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/records/batch"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (!id) {
    result$records$id <- NULL
  }
  
  if (any(grepl("formula", fields, ignore.case = TRUE)) && simplify_formula) {
    result$records$formula <- gsub(pattern = "[[:punct:]]", replacement = "", x = result$records$formula)
  }
  
  if (coerce) {
    result <- result$records
  }
  
  result
}
