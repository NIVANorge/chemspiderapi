#' @title Convert between chemical identifiers
#' @description Functionality to convert between different chemical identifier formats: InChI, InChIKey, Mol, and SMILES.
#' @details "Specify the input format as a string called 'inputFormat', and the output as a string called 'outputFormat'. Allowed conversions: from InChI to InChI Key, from InChI to Mol file, from InChI to SMILES, from InChIKey to InChI, from InChI Key to Mol file, from Mol file to InChI, from Mol file to InChI Key, from SMILES to InChI."\cr
#' \cr
#' The identifier names are NOT case sensitive!
#' @param input A character string in the specified \code{inputFormat}.
#' @param inputFormat A character string indicating which format the input has. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param outputFormat A character string indicating which type of output is desired. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param coerce \code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.
#' @param simplify \code{logical}: should the results be simplified to a vector? Defaults to \code{FALSE}.
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
post_convert <- function(input, inputFormat, outputFormat, apikey, coerce = FALSE, simplify = FALSE) {
  
  .check_format(input, inputFormat, outputFormat)
  
  .check_apikey(apikey)
  
  .check_coerce(coerce)
  
  .check_simplify(simplify)
  
  data <- list("input" = input, 
               "inputFormat" = inputFormat, 
               "outputFormat" = outputFormat)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- Sys.getenv("POST_CONVERT_URL",
                    "https://api.rsc.org/compounds/v1/tools/convert")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (coerce) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  if (simplify) {
    result <- unlist(result, use.names = FALSE)
  }
  
  result
}
