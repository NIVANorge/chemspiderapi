#' GET the record details for a compound from ChemSpider
#'
#' This function is used to return record details from ChemSpider.
#'
#' The available options for \code{fields} are: \code{"SMILES"}, \code{"Formula"}, \code{"AverageMass"}, \code{"MolecularWeight"}, \code{"MonoisotopicMass"}, \code{"NominalMass"}, \code{"CommonName"}, \code{"ReferenceCount"}, \code{"DataSourceCount"}, \code{"PubMedCount"}, \code{"RSCCount"}, \code{"Mol2D"}, \code{"Mol3D"}, and \code{"all"}.\cr
#' \cr
#' If successful, returns either a \code{data.frame} of results from the query with the requested fields; if only one paraemeter is returned, this is then transformed into a vector.\cr
#' \cr
#' If not successful, it returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_*()}.
#'
#' @param recordId A valid (integer) ChemSpider ID.
#' @param fields Either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param id \code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output?
#' @return A \code{data.frame} if multiple columns are returned, or a (named) vector of the appropriate type if only one \code{field} is returned.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details}
#' @examples \dontrun{
#' ## GET the record details for aspirin
#' recordId <- 2157L
#' apikey <- "A valid 32-character Chemspider API key"
#' get_recordId_details(recordId = recordId, 
#'                      fields = c("SMILES", "Formula", "MolecularWeight", "CommonName"), 
#'                      apikey = apikey)
#' }
#' @export
get_recordId_details <- function(recordId, fields = "all", apikey, id = TRUE) {
  
  if (is.null(recordId)) {
    stop("No valid \"recordId\" provided.", call. = FALSE)
  }
  
  if (length(recordId) > 1L) {
    stop("This function can only handle a single \"recordId\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }

  if (is.na(as.integer(recordId)) == TRUE) {
    stop("Please provide a valid (integer) \"recordId\".", call. = FALSE)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA)
  }

  if (fields != "all" && sum(tolower(fields) %in% c("smiles", "formula", "averagemass", "molecularweight", "monoisotopicmass", "nominalmass", "commonname", "referencecount", "datasourcecount", "pubmedcount", "rsccount", "mol2d", "mol3d")) < length(fields)) {
    warning("One or more \"fields\" are not valid; returning \"NA\".", call. = FALSE)
    return(NA)
  }

  if (length(fields) == 1L && fields == "all") {
      fields <- "SMILES,Formula,AverageMass,MolecularWeight,MonoisotopicMass,NominalMass,CommonName,ReferenceCount,DataSourceCount,PubMedCount,RSCCount,Mol2D,Mol3D"
  }

  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }

  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA)
  }

  fields <- paste(fields, collapse = ",")

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/details?fields=", fields)

  curlHandle <- curl::new_handle()

  curl::handle_setopt(curlHandle, customrequest = "GET")

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
    return(NA)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  if (id == FALSE) {
    result$id <- NULL
  }

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  return(result)
}
