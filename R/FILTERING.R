###################
# post_inchikey() #
###################

#' POST an InChIKey as ChemSpider query
#'
#' Functionality to POST an InChIKey to obtain a \code{queryId} for use in \code{chemspiderapi::get_status()} and \code{chemspiderapi::get_results()}.
#'
#' The validity criteria for InChIKeys are outlined here: \url{https://www.inchi-trust.org/technical-faq/#13.1}. If certain criteria are not met by the input \code{inchikey}, \code{chemspideR::post_inchikey()} returns a warning message and \code{NA} (and does not perform an API query). In the case of a non-standard \code{inchikey}, a warning is issued but the query will be performed.\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#'
#' @param inchikey A valid 27-character InChIKey; see Details.
#' @param apikey A valid 32-character string with a valid key for ChemSpider's API services.
#' @return A 36-character queryId string
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchikey}
#' @examples
#' \dontrun{
#' ## POST the InChIKey of aspirin to obtain a queryId
#' inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#' apikey <- "A valid 32-character Chemspider API key"
#' post_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @export
post_inchikey <- function(inchikey, apikey) {
  
  if (is.na(inchikey)) {
    warning("No valid \"inchikey\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(inchikey) > 1L) {
    warning("This function can only handle a single \"inchikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(inchikey) != "character") {
    warning("The \"inchikey\" should be a 27-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(inchikey) != 27L) {
    warning("The provided \"inchikey\" is not a 27-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(unlist(strsplit(inchikey, split = "-"))) != 3L) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14L) {
    warning("The first part of the \"inchikey\" should be 14 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10L) {
    warning("The second part of the \"inchikey\" should be 10 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1L) {
    warning("The third part of the \"inchikey\" should be 1 character long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }

  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing API query regardless.", call. = FALSE)
  }

  curlData <- list(inchikey = inchikey)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchikey"

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
    
    if (result$status_code == 413L) {
      error_message <- "\nChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
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

  return(result)
}


#########################
# get_queryId_results() #
#########################

#' GET the results of a ChemSpider query
#'
#' This function is used to retrieve the results of a ChemSpider query after \code{chemspiderapi::get_status()} returns \code{"Complete"}.
#'
#' Before running \code{chemspiderapi::get_results()}, make sure \code{chemspiderapi::get_status()} returns \code{"Complete"}. In fact, this function will return \code{"NA"} if the status is not \code{"Complete"}.\cr
#' \cr
#' If the results have been truncated because there were more results than the maximum number of results permitted (by default, 10,000), a warning is issued. If this happens, you can split your requests into smaller batches.\cr
#' \cr
#' To batch your requests, call this function with two optional parameters, \code{start} and \code{count}. Both take integer values. \code{start} is the number of the record to start with (zero-based), and \code{count} is the number of records to return. For example, to request results 200-300, use \code{start = 200L} and \code{count = 100L}.\cr
#' \cr
#' If successful, returns a data frame with the query results; in case the response is a single value, e.g., a ChemSpider ID, it is returned as single vector.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_*()}.
#'
#' @param queryId A valid 36-character ChemSpider query ID string; see Details.
#' @param start Optional: An integer value giving the position from which to start the retrival of query results. See Details.
#' @param count Optional: An integer value giving the the number of query results to retrieve. See Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param status A character string indicating the query status as returned by \code{chemspiderapi::get_queryId_status()}
#' @return A character vector indicating the status of the query; see Details.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results}
#' @examples
#' \dontrun{
#' ## GET the results of a query from ChemSpider
#' queryId <- "A valid 36-character Chemspider query ID"
#' apikey <- "A valid 32-character Chemspider API key"
#' status <- get_queryId_status(queryId = queryId, apikey = apikey)
#' get_queryId_results(queryId = queryId, status = status, apikey = apikey)
#' }
#' @export
get_queryId_results <- function(queryId, status, start = NULL, count = NULL, apikey) {
  
  if (is.na(queryId)) {
    warning("No valid \"queryId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }
  
  if (length(queryId) > 1L) {

    warning("This function can only handle a single ChemSpider \"queryId\" entry; returning \"NA\".\nMaybe you are looking for chemspideR::get_formula_batch_status() or chemspideR::get_mass_batch_status()?\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)

    return(NA_integer_)
  }

  if (typeof(queryId) != "character") {
    warning("The ChemSpider \"queryId\" should be a 36-character string; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    warning("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    warning("The first part of the ChemSpider \"queryId\" should be 8 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    warning("The second part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    warning("The third part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    warning("The fourth part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    warning("The fifth part of the ChemSpider \"queryId\" should be 12 characters long; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (is.null(status)) {
    warning("No ChemSpider query \"status\" provided; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (status != "Complete") {
    warning("Query computation not yet completet; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (!is.null(start) && is.na(as.integer(start))) {
    warning("Please use a valid integer \"start\" value; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (!is.null(count) && is.na(as.integer(count))) {
    warning("Please use a valid integer \"count\" value; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_integer_)
  }

  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_integer_)
  }

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  if (is.null(start) && is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results")
  }

  if (!is.null(start) && is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?start=", start)
  }

  if (is.null(start) && !is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?count=", count)
  }

  if (!is.null(start) && !is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?start=", start, "&count=", count)
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
    return(NA_integer_)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)

  if (result$limitedToMaxAllowed == TRUE) {
    warning("The query has resulted in > 10'000 entries. Only the first 10'000 are returned.\nConsider splitting this request using \"start\" and \"count\".", call. = FALSE)
  }

  result <- data.frame(results = result$results, stringsAsFactors = FALSE)

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  return(result)
}


########################
# get_queryId_status() #
########################

#' GET the status of a ChemSpider query
#'
#' This function is used to return the status of a query from ChemSpider before \code{chemspiderapi::get_results()} is called.
#'
#' Call this endpoint with a \code{queryId} obtained from a previous POST query,e.g., \code{chemspiderapi::post_inchikey()}.
#' \cr
#' If the query is still ongoing, returns a warning and a character vector of the query status as \code{Incomplete}. It is recommended to wait at least ten seconds before checking the status again.\cr
#' \cr
#' If the query is finalized, returns a data frame of the query status with \code{status}, \code{count} and \code{message}. The \code{status} can be either \code{Complete}, \code{Suspended}, \code{Failed}, or \code{Not Found}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{A status of Suspended can be caused if the results could not be compiled within a reasonable amount of time. Create a new filter request with more restrictive parameters.}\cr
#' \cr
#' \emph{A status of Failed can be caused if the backend system could not compile the results. Create a new filter request and, if the same outcome occurs, apply more restrictive parameters.}\cr
#' \cr
#' \emph{A status of Not Found can be caused if the Query ID has not been registered or has expired. Create a new filter request.}\cr
#' \cr
#' If both \code{count} and \code{message} are set to \code{FALSE}, \code{chemspiderapi::get_status()} returns the \code{status} as character vector. This is recommended for functional programming approaches.\cr
#' \cr
#' If the status is \code{Complete}, the results of the query can be obtained from \code{chemspiderapi::get_results()}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#'
#' @param queryId A valid 36-character ChemSpider query ID string; see Details.
#' @param count \code{logical}: Should the count of the results be returned?
#' @param message \code{logical}: Should the message be returned?
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character vector indicating the status of the query; see Details.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/status}
#' @examples
#' \dontrun{
#' ## GET the status of a query from ChemSpider
#' queryId <- "A valid 36-character Chemspider query ID"
#' apikey <- "A valid 32-character Chemspider API key"
#' get_queryId_status(queryId = queryId, apikey = apikey)
#' }
#' @export
get_queryId_status <- function(queryId, count = TRUE, message = TRUE, apikey) {
  
  if (is.na(queryId)) {
    warning("No valid \"queryId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(queryId) > 1L) {
    warning("This function can only handle a single ChemSpider \"queryId\" entry; returning \"NA\".\nMaybe you are looking for chemspideR::get_formula_batch_status() or chemspideR::get_mass_batch_status()?\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(queryId) != "character") {
    warning("The ChemSpider \"queryId\" should be a 36-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    warning("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    warning("The first part of the ChemSpider \"queryId\" should be 8 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    warning("The second part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    warning("The third part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    warning("The fourth part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    warning("The fifth part of the ChemSpider \"queryId\" should be 12 characters long; returning \"NA\".", call. = FALSE)
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

  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/status")

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
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  if (count == FALSE) {
    result$count <- NULL
  }

  if (message == FALSE) {
    result$message <- NULL
  }

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  return(result)
}


#################
# post_smiles() #
#################

#' POST a SMILES string
#' 
#' Functionality to POST a SMILES string to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryID_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param smiles A SMILES character string.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a query ID character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/smiles}
#' @examples 
#' \dontrun{
#' ## POST the SMILES string of aspirin to get a queryId
#' smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_smiles(smiles = smiles, apikey = apikey)
#' }
#' @export
post_smiles <- function(smiles, apikey) {
  if (is.na(smiles)) {
    warning("No \"smiles\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(smiles) > 1) {
    warning("This function can only handle a single \"smiles\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
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
  
  curlData <- list(smiles = smiles)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/smiles"
  
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
  result <- unlist(result)
  
  return(result)
}


################
# post_inchi() #
################

#' POST an InChI string
#' 
#' Functionality to POST an InChI string to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryID_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' The validity criteria for InChI strings are outlined here: \url{https://www.inchi-trust.org/technical-faq/#2.8}. If certain criteria are not met by the input \code{inchi}, \code{chemspiderapi::post_inchi()} returns a warning message and \code{NA}, and does not perform an API query.\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param inchi A valid InChI string; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Return a query ID as character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchi} 
#' @examples 
#' \dontrun{
#' ## POST the InChI string of Aspirin to get a queryId
#' inchi <- "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_inchi(inchi = inchi, apikey = apikey)}
#' @export
post_inchi <- function(inchi, apikey) {
  
  if (is.na(inchi)) {
    warning("No valid \"inchi\" string provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(inchi) > 1) {
    warning("This function can only handle a single \"inchi\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (substr(tolower(inchi), start = 1L, stop = 7L) != "inchi=1") {
    warning("This is not a valid \"inchi\" string because \"InChI=1\" is missing in the beginning; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (substr(tolower(inchi), start = 8L, stop = 8L) != "s") {
    warning("This is not a standard \"inchi\" string; performing API query regardless.", call. = FALSE)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  
  curlData <- list(inchi = inchi)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchi"
  
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
    
    if (result$status_code == 413L) {
      error_message <- "\nChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
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


###############
# post_name() #
###############

#' POST a name to obtain a ChemSpider query ID
#' 
#' Functionality to POST the name of a compound and obtain a ChemSpider query ID for subsequent use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' Allowed entries for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Allowed entries for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' If successful, returns a 36-character \code{queryId} as a character vector. The \code{queryId} can be used in \code{chemspiderapi::get_queryId_status()} and \code{chemspider::get_queryId_results()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.\cr
#' 
#' @param name A character string of the compound name.
#' @param orderBy A character string indicating by which parameter the results should be arranged (NOT case sensitive); see Details.
#' @param orderDirection A character string indicating which in which direction the results should be arranged (NOT case sensitive); see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the query ID as character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/name}
#' @examples 
#' \dontrun{
#' ## POST the name of aspirin to obtain a query ID
#' name <- "aspirin"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_name(name = name, apikey = apikey)
#' }
#' @export
post_name <- function(name, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(name)) {
    warning("No \"name\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(name) > 1) {
    warning("This function can only handle a single \"name\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderBy) > 1) {
    warning("Only a single \"orderBy\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("Please provide a valid input for \"orderBy\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderDirection) > 1) {
    warning("Only a single \"orderDirection\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\"; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  
  curlData <- list(name = name, orderBy = orderBy, orderDirection = orderDirection)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/name"
  
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
    
    if (result$status_code == 413L) {
      error_message <- "\nChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
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


#############################
# get_queryId_results_sdf() #
#############################

#' GET the gzipped .sdf file for a ChemSpider query
#' 
#' This function is used to download a single .sdf file from ChemSpider after \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}.
#' 
#' Call this function after \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}.\cr
#' \cr
#' If successful, returns a character vector of results from the query, a gzipped .sdf file.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param queryId A valid 36-character ChemSpider \code{queryId}.
#' @param status A character string indicating the query status as returned by \code{chemspiderapi::get_queryId_status()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a (base64-encoded) character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results/sdf}
#' @examples 
#' \dontrun{
#' ## GET a gzipped .sdf file
#' queryId <- "a valid 36-character ChemSpider apikey"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_queryId_results_sdf(queryId = queryId, apikey = apikey)
#' }
#' @export
get_queryId_results_sdf <- function(queryId, status, apikey) {
  
  if (is.na(queryId)) {
    warning("No valid \"queryId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(queryId) > 1L) {
    warning("This function can only handle a single ChemSpider \"queryId\" entry; returning \"NA\".\nMaybe you are looking for chemspideR::get_formula_batch_status() or chemspideR::get_mass_batch_status()?\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(queryId) != "character") {
    warning("The ChemSpider \"queryId\" should be a 36-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    warning("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    warning("The first part of the ChemSpider \"queryId\" should be 8 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    warning("The second part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    warning("The third part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    warning("The fourth part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    warning("The fifth part of the ChemSpider \"queryId\" should be 12 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.null(status)) {
    warning("No ChemSpider query \"status\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (status != "Complete") {
    warning("Query computation not yet completet; returning \"NA\".", call. = FALSE)
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
  
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results/sdf")
  
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
    return(NA_integer_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result$results)
  
  return(result)
}


##################
# post_formula() #
##################

#' POST a chemical formula to obtain a query ID
#' 
#' Functionality to POST a formula to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
#' \cr
#' Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param formula A character string of a chemical formula.
#' @param dataSources Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character vector containing the query ID
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula}
#' @examples 
#' \dontrun{
#' ## POST the formula of Aspirin to get a query ID
#' formula <- "C9H8O4"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_formula(formula = formula, apikey = apikey)
#' }
#' @export
post_formula <- function(formula, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(formula)) {
    warning("No \"formula\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(formula) > 1) {
    warning("This function can only handle individual \"formula\" entries; returning NA.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("No valid \"orderBy\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("No valid \"orderDirection\" provided; returning \"NA\".", call. = FALSE)
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
  
  if (!is.null(dataSources) && length(dataSources) == 1) {
    dataSources <- I(dataSources)
  }
  
  curlData <- list(formula = formula, orderBy = orderBy, orderDirection = orderDirection)
  
  if (!is.null(dataSources)) {
    curlData <- list(formula = formula, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  }
  
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/formula"
  
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
  result <- unlist(result)
  
  return(result)
}
