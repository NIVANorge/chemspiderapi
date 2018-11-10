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
#' @return A \code{data.frame} if multiple columns are returned, or a named vector of the appropriate type if only one \code{field} is returned.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details}
#' @examples
#' \dontrun{
#' ## GET the record details for aspirin
#' recordId <- 2157L
#' apikey <- "A valid 32-character Chemspider API key"
#' get_recordId_details(recordId = recordId, 
#'                      fields = c("SMILES", "Formula", "MolecularWeight", "CommonName"), 
#'                      apikey = apikey)
#' }
#' @export
get_recordId_details <- function(recordId, fields = "all", apikey, id = TRUE) {
  
  if (is.na(recordId)) {
    warning("No valid \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA)
  }
  
  if (length(recordId) > 1L) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }

  if (is.na(as.integer(recordId)) == TRUE) {
    warning("Please provide a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
    return(NA)
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

#' GET a .png image of a ChemSpider record ID
#' 
#' This function is used to obtain a .png image file of a ChemSpider record ID, e.g., after \code{chemspiderapi::get_queryID_results()}.
#' 
#' If succesfull, returns either a base64-decoded character vector (\code{png = TRUE}) or a numeric (double) array (\code{png = TRUE}). Either of them can be written to the hard drive using \code{chemspiderapi::write_image()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param png \code{logical}: Should the base64-encoded character string be converted to a (raw) .png image?
#' @return Either a base64-encoded character string (\code{png == FALSE}) or a raw array (\code{png == TRUE}).
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/image} 
#' @examples
#' \dontrun{
#' ## GET the .png image for aspirin
#' recordId <- 2157L
#' apikey <- "a_valid_ChemSpider_API_key"
#' get_recordId_image(recordId = recordId, apikey = apikey)
#' get_recordId_image(recordId = recordId, apikey = apikey, png = TRUE)
#' }
#' @export 
get_recordId_image <- function(recordId, apikey, png = FALSE) {
  if (is.na(recordId)) {
    warning("No \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA)
  }
  
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }
  
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
    return(NA)
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
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  
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
  result <- unlist(result)
  
  if (png == TRUE) {
    result <- jsonlite::base64_dec(result)
    result <- png::readPNG(result)
  }
  
  return(result)
}

#' GET the .MOL of a ChemSpider record
#' 
#' This function is used to download a single .MOL file from ChemSpider.
#' 
#' Call this endpoint with \code{recordId} as an integer.\cr
#' \cr
#' If successful, returns a character vector of result of the query, i.e., a .MOL file. This character vector can be written to the hard drive using \code{chemspiderapi::write_mol()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#'  
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character string containing the (human-readable) .MOL file.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/mol} 
#' @examples
#' \dontrun{
#' ## GET the .MOL file for aspirin
#' recordId <- 2157L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_mol(recordId = recordId, apikey = apikey)
#' }
#' @export
get_recordId_mol <- function(recordId, apikey) {
  
  if (is.na(recordId)) {
    warning("No \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
    }
  
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(as.integer(recordId))) {
    warning("Please provide a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
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
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/mol")
  
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
    return(NA_character_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)

    return(result)
}

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
get_recordId_externalreferences <- function(recordId, dataSources = NULL, apikey, source = TRUE, sourceUrl = TRUE, externalUrl = TRUE) {
  if (is.na(recordId)) {
    warning("No \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid \"recordId\"; returning NA.", call. = FALSE)
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
  
  if (!is.null(dataSources)) {
    dataSources <- paste(dataSources, collapse = ",")
  }
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  if (!is.null(dataSources)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences?dataSources=", dataSources)
  }
  
  else {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences")
  }
  
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
    return(NA_character_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  if (source == FALSE) {
    result$source <- NULL
  }
  
  if (sourceUrl == FALSE) {
    result$sourceUrl <- NULL
  }
  
  if (externalUrl == FALSE) {
    result$externalUrl <- NULL
  }
  
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  
  return(result)
}

