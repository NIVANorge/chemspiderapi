#' POST a batch of ChemSpider IDs
#' 
#' POST a batch query of up to 100 ChemSpider record IDs.
#' 
#' The available options for \code{fields} are: \code{"SMILES"}, \code{"Formula"}, \code{"AverageMass"}, \code{"MolecularWeight"}, \code{"MonoisotopicMass"}, \code{"NominalMass"}, \code{"CommonName"}, \code{"ReferenceCount"}, \code{"DataSourceCount"}, \code{"PubMedCount"}, \code{"RSCCount"}, \code{"Mol2D"}, \code{"Mol3D"}, and \code{"all"} (default).\cr
#' \cr
#' If a \code{recordId} is not found, it is silently dropped.\cr
#' \cr
#' If successful, returns a data frame with each row being a \code{recordId} and each column being one of \code{fields}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param recordIds A vector of integer ChemSpider IDs.
#' @param fields A character string indicating which fields to return; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the record ID be returned (ChemSpider default)?
#' @return A data frame (if multiple fields are returned), or a vector of adequate type if only one field is requiered.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/records/batch}
#' @examples
#' \dontrun{
#' ## POST a query for the first 50 ChemSpider entries.
#' recordIds <- 1:50
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_batch(recordIds = recordIds, apikey = apikey, id = FALSE)
#' }
#' @export
post_batch <- function(recordIds, fields = "all", apikey, id = TRUE) {
  
  check_recordIds(recordIds)
  
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
  
  data <- list(recordIds = recordIds, fields = fields)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/records/batch"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(unlist(result), stringsAsFactors = FALSE)
  
  if (isFALSE(id)) {
    result$id <- NULL
  }
  
  if (ncol(result) == 1) {
    result <- unlist(result)
    result <- unname(result)
  }
  
  result
}
