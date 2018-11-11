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
  if (is.na(recordIds)) {
    warning("No \"recordIds\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(recordIds) == 1) {
    warning("This function is meant for handling multiple \"recordIds\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(recordIds) > 100) {
    warning("Only up to 100 \"recordIds\" are allowed; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(fields) == 1 && fields == "all") {
    fields <- c("SMILES", "Formula", "AverageMass", "MolecularWeight", "MonoisotopicMass", "NominalMass", "CommonName", "ReferenceCount", "DataSourceCount", "PubMedCount", "RSCCount", "Mol2D", "Mol3D")
  }
  
  if (length(fields) == 1) {
    fields <- I(fields)
  }
  
  curlData <- list(recordIds = recordIds, fields = fields)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/records/batch"
  
  curlHandle <- curl::new_handle()
  
  curl::handle_setopt(curlHandle, customrequest = "POST", postfields = curlData)
  
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  
  if (result$status_code != 200L) {
    
    error_message <- "\nNo ChemSpider Error Details were provided."
    
    if (result$status_code == 400L) {
      error_message <- "\nChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (result$status_code == 401L) {
      error_message <- "\nChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (result$status_code == 404L) {
      error_message <- "\nChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (result$status_code == 405L) {
      error_message <- "\nChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (result$status_code == 429L) {
      error_message <- "\nChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (result$status_code == 500L) {
      error_message <- "\nChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (result$status_code == 503L) {
      error_message <- "\nChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", error_message)
    
    warning(message, call. = FALSE)
    return(NA_character_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(unlist(result), stringsAsFactors = FALSE)
  
  if (id == FALSE) {
    result$id <- NULL
  }
  
  if (ncol(result) == 1) {
    result <- unlist(result)
    result <- unname(result)
  }
  return(result)
}
