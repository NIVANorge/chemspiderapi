#' @title Convert between chemical identifiers
#' @description Functionality to convert between different chemical identifier formats: InChI, InChIKey, Mol, and SMILES.
#' @details Submit an identifier (SMILES, InChI, InChIKey or Mol) as a character string and return an identifier in another format (SMILES, InChI, InChIKey or Mol).\cr
#' \cr
#' Allowed conversions:\cr
#' \cr
#' from \code{InChI} to \code{InChIKey}\cr
#' \cr
#' from \code{InChI} to \code{Mol}\cr
#' \cr
#' from \code{InChI} to \code{SMILES}\cr
#' \cr
#' from \code{InChIKey} to \code{InChI}\cr
#' \cr
#' from \code{InChIKey} to \code{Mol}\cr
#' \cr
#' from \code{Mol} to \code{InChI}\cr
#' \cr
#' from \code{Mol} to \code{InChIKey}\cr
#' \cr
#' from \code{SMILES} to \code{InChI}\cr
#' \cr
#' Note: The identifier names are NOT case sensitive!\cr
#' \cr
#' If successful, performs the desired conversion and stores the result as a named ("output") character string.\cr
#' @param input A character string in the specified \code{inputFormat}.
#' @param inputFormat A character string indicating which format the input has. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param outputFormat A character string indicating which type of output is desired. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character string with the desired converted identifier
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/convert}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Convert the InChI string of caffeine to a SMILES formula
#' input <- "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3"
#' inputFormat <- "InChI"
#' outputFormat <- "SMILES"
#' apikey <- "A valid 32-character Chemspider API key"
#' post_convert(input = input, inputFormat = inputFormat, outputFormat = outputFormat, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export    
post_convert <- function(input, inputFormat, outputFormat, apikey) {
  
  if (is.null(input)) {
    stop("Please provide an \"input\".", call. = FALSE)
  }
  
  if (is.null(inputFormat)) {
    stop("Please provide an \"inputFormat\".", call. = FALSE)
  }
  
  if (is.null(outputFormat)) {
    stop("Please provide an \"outputFormat\".", call. = FALSE)
  }
  
  if (length(input) > 1) {
    stop("Please provide a single \"input\" string.\nFor functional programming, try using chemspider::post_convert() in apply() or purrr::map_chr().", call. = FALSE)
  }

  if (!any(tolower(inputFormat) %in% c("inchi", "inchikey", "smiles", "mol"))) {
    stop("Please provide a valid \"inputFormat\". See Documentation for details.", call. = FALSE)
  }
  
  if (!any(tolower(outputFormat) %in% c("inchi", "inchikey", "smiles", "mol"))) {
    stop("Please provide a valid \"outputFormat\". See Documentation for details.", call. = FALSE)
    }
  
  if (tolower(inputFormat) == "inchi" && tolower(substr(input, start = 1L, stop = 7L)) != "inchi=1") {
    stop("Please provide a valid \"InChI\" as \"input\".\nAn \"InChI\" should start with \"InChI=1\".", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchi" && tolower(unlist(substr(inchi, start = 8L, stop = 8L))) != "s") {
    warning("The provided InChI string is not a standard InChI; performing API query regardless.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(input) != 27L) {
    stop("Please provide a valid \"InChIKey\" as \"input\".\nAn \"InChIKey\" should be a 27-character vector.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && length(unlist(strsplit(input, split = "-"))) != 3L) {
    stop("Please provide a valid \"InChIKey\" as \"input\".\nAn \"InChIKey\" should be hyphen-divided into three parts.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[1]) != 14L) {
    stop("Please provide a valid \"InChIKey\" as \"input\".\nThe first part of an \"InChIKey\" should be a 14-character string.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[2]) != 10L) {
    stop("Please provide a valid \"InChIKey\" as \"input\".\nThe second part of an \"InChIKey\" should be a ten-character string.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[3]) != 1L) {
    stop("Please provide a valid \"InChIKey\" as \"input\".\nThe third part of an \"InChIKey\" should be a one-character string.", call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchikey" && substr(unlist(strsplit(input, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("The provided \"InChIKey\" is not a standard \"InChIKey\".", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  curl_data <- list("input" = input, "inputFormat" = inputFormat, "outputFormat" = outputFormat)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  curl_url <- "https://api.rsc.org/compounds/v1/tools/convert"
  curl_handle <- curl::new_handle()
  curl::handle_setopt(curl_handle, customrequest = "POST", postfields = curl_data)
  curl::handle_setheaders(curl_handle, .list = curl_header)
  raw_result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)
  
  if (raw_result$status_code != 200L) {
    
    if (raw_result$status_code == 400L) {
      error_message <- "ChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    if (raw_result$status_code == 401L) {
      error_message <- "ChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    if (raw_result$status_code == 404L) {
      error_message <- "ChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    if (raw_result$status_code == 405L) {
      error_message <- "ChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    if (raw_result$status_code == 413L) {
      error_message <- "ChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
    }
    if (raw_result$status_code == 429L) {
      error_message <- "ChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    if (raw_result$status_code == 500L) {
      error_message <- "ChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    if (raw_result$status_code == 503L) {
      error_message <- "ChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    if (!any(raw_result$status_code %in% c(400L, 401L, 404L, 405L, 413L, 429L, 500L, 503L))) {
      error_message <- "No ChemSpider Error Details were provided."
    }
    
    message <- paste0("No valid information was retrieved.\nCarfully check the validity of the provided ChemSpider \"apikey\".\n", error_message)
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
