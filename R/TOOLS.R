#' Convert chemical identifiers
#' 
#' Functionality to convert between different chemical identifier formats: InChI, InChIKey, Mol, and SMILES.
#' 
#' Submit an identifier (SMILES, InChI, InChIKey or Mol) as a string and return an identifier in another format (SMILES, InChI, InChIKey or Mol).\cr
#' \cr
#' Allowed conversions:\cr
#' \cr
#' from \code{InChI} to \code{InChIKey}\cr
#' \cr
#' from \code{InChI} to \code{Mol} file\cr
#' \cr
#' from \code{InChI} to \code{SMILES}\cr
#' \cr
#' from \code{InChIKey} to \code{InChI}\cr
#' \cr
#' from \code{InChIKey} to \code{Mol} file\cr
#' \cr
#' from \code{Mol} file to \code{InChI}\cr
#' \cr
#' from \code{Mol} file to \code{InChIKey}\cr
#' \cr
#' from \code{SMILES} to \code{InChI}\cr
#' \cr
#' Note: The identifier names are NOT case sensitive!\cr
#' \cr
#' If successful, performs the desired conversion and stores the result as a named ("output") character string.\cr
#' \cr
#' If not successful, it returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}
#' 
#' @param input A character string in the specified \code{inputFormat}.
#' @param inputFormat A character string indicating which format the input has. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param outputFormat A character string indicating which type of output is desired. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param apikey A valid 32-character string with a valid key for ChemSpider's API services.
#' @return A character string with the desired converted identifier
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/convert}
#' @examples
#' ## convert the InChI string of aspirin to SMILES.
#' # input <- "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
#' # inputFormat <- "InChI"
#' # outputFormat <- "SMILES"
#' # apikey <- "A valid 32-character Chemspider API key"
#' # post_convert(input = input, inputFormat = inputFormat, outputFormat = outputFormat,
#' #              apikey = apikey)
#' @export    
post_convert <- function(input, inputFormat, outputFormat, apikey) {
  
  if (length(input) > 1) {
    warning("This function can only handle individual (\"input\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (sum(tolower(inputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) != length(inputFormat)) {
    warning("This entry of \"inputFormat\" is not a valid ChemSpider input format; returning \"NA\".\nMaybe a spelling error?", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(outputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) != length(outputFormat)) {
    warning("This entry of \"outputFormat\" is not a valid ChemSpider output format.\nMaybe a spelling error?", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchi" && tolower(substr(input, start = 1L, stop = 7L)) != "inchi=1") {
    warning("Provided \"input\" is not a valid InChI string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(input) != 27L) {
    warning("The provided \"inchikey\" should be a 27-character vector; not performing API query.", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && length(unlist(strsplit(input, split = "-"))) != 3L) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; not performing API query.", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[1]) != 14L) {
    warning("The first part of the \"inchikey\" should be 14 characters long; not performing API query.", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[2]) != 10L) {
    warning("The first part of the \"inchikey\" should be 10 characters long; not performing API query.", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && nchar(unlist(strsplit(input, split = "-"))[3]) != 1L) {
    warning("The third part of the \"inchikey\" should be 1 character long; not performing API query.", call. = FALSE)
    return(NA_character_)
  }
  
  if (tolower(inputFormat) == "inchikey" && substr(unlist(strsplit(input, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing query regardless.", call. = FALSE)
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
  
  curlData <- list(input = input, inputFormat = inputFormat, outputFormat = outputFormat)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/tools/convert"
  
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
