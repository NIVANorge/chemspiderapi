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
