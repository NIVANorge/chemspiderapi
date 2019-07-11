#' @title POST an InChIKey as ChemSpider query
#' @description Functionality to POST an InChIKey to obtain a \code{queryId} for use in \code{chemspiderapi::get_status()} and \code{chemspiderapi::get_results()}.
#' @details The validity criteria for InChIKeys are outlined here: \url{https://www.inchi-trust.org/technical-faq/#13.1}. If certain criteria are not met by the input \code{inchikey}, \code{chemspideR::post_inchikey()} returns a warning message and \code{NA} (and does not perform an API query). In the case of a non-standard \code{inchikey}, a warning is issued but the query will be performed.\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.
#' @param inchikey A valid 27-character InChIKey; see Details.
#' @param apikey A valid 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchikey}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST the InChIKey of aspirin to obtain a queryId
#' inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#' apikey <- "A valid 32-character Chemspider API key"
#' post_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_inchikey <- function(inchikey, apikey) {
  
  if (is.null(inchikey)) {
    stop("No \"inchikey\" provided.", call. = FALSE)
  }

  if (length(inchikey) > 1L) {
    stop("This function can only handle a single \"inchikey\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }

  if (typeof(inchikey) != "character") {
    stop("The \"inchikey\" should be a 27-character string.", call. = FALSE)
  }

  if (nchar(inchikey) != 27L) {
    stop("The provided \"inchikey\" is not a 27-character string.", call. = FALSE)
  }

  if (length(unlist(strsplit(inchikey, split = "-"))) != 3L) {
    stop("The provided \"inchikey\" must be hyphen-divided into three parts.", call. = FALSE)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14L) {
    stop("The first part of the \"inchikey\" should be 14 characters long.", call. = FALSE)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10L) {
    stop("The second part of the \"inchikey\" should be 10 characters long.", call. = FALSE)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1L) {
    stop("The third part of the \"inchikey\" should be 1 character long.", call. = FALSE)
  }

  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }

  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
  }

  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing API query regardless.", call. = FALSE)
  }

  curl_data <- list("inchikey" = inchikey)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)

  curl_header <- list("Content-Type" = "", "apikey" = apikey)

  curl_url <- "https://api.rsc.org/compounds/v1/filter/inchikey"

  curl_handle <- curl::new_handle()

  curl::handle_setopt(curl_handle, customrequest = "POST", postfields = curl_data)

  curl::handle_setheaders(curl_handle, .list = curl_header)

  raw_result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)

  if (raw_result$status_code != 200L) {
    
    error_message <- "\nNo ChemSpider Error Details were provided."
    
    if (raw_result$status_code == 400L) {
      error_message <- "\nChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (raw_result$status_code == 401L) {
      error_message <- "\nChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (raw_result$status_code == 404L) {
      error_message <- "\nChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 405L) {
      error_message <- "\nChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 413L) {
      error_message <- "\nChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 429L) {
      error_message <- "\nChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (raw_result$status_code == 500L) {
      error_message <- "\nChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (raw_result$status_code == 503L) {
      error_message <- "\nChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", error_message)

    stop(message, call. = FALSE)
  }

  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)

  result
}
