#' @title Post a batch of ChemSpider record IDs
#' @description Post a batch query of up to 100 ChemSpider record IDs.
#' @details "The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."\cr
#' \cr
#' If a \code{recordId} is not found, it is silently dropped.
#' @param recordIds A vector of integer ChemSpider IDs.
#' @param fields A character string indicating which fields to return; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output? Defaults to \code{FALSE}.
#' @param simplify_formula \code{logical}: Should formula strings be simplified? Defaults to \code{TRUE}.
#' @return A data frame (if multiple fields are returned), or a vector of adequate type if only one field is requiered.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/records/batch}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Post a query for the first 50 ChemSpider entries.
#' recordIds <- 1:50
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_batch(recordIds = recordIds, apikey = apikey)
#' }
#' @export
post_batch <- function(recordIds, fields = "all", apikey, id = TRUE, simplify_formula = TRUE) {
  
  check_recordIds(recordIds)
  
  check_fields(fields)
  
  check_apikey(apikey)
  
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
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result$records, stringsAsFactors = FALSE)
  
  if (isFALSE(id)) {
    result$id <- NULL
  }
  
  if (grepl("formula", fields, ignore.case = TRUE) && isTRUE(simplify_formula)) {
    result$formula <- gsub(pattern = "[[:punct:]]", replacement = "", x = result$formula)
  }
  
  check_result(result)
  
  result
}
