#' @title Validate an InChIKey
#' @description Functionality to check the validity of an InChIKey.
#' @details If successful (i.e., the InChIKey is valid), returns \code{TRUE}.\cr
#' \cr
#' If not successful (i.e., the InChIKey is not valid), returns \code{FALSE}.\cr
#' \cr
#' Before this functions performs an API query, it runs quality checking as lined out at \url{https://www.inchi-trust.org/technical-faq/#13.1}. If an InChIKey is ruled out based on these criteria, it returns \code{FALSE} with a warning message.
#' @param inchikey A 27-character InChIKey to be validated.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A logical indicating the validity of the POSTed InChIKey
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/validate/inchikey}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## validate the InChIKey of aspirin
#' inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#' apikey <- "a_valid_ChemSpider_API_key"
#' post_validate_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_validate_inchikey <- function(inchikey, apikey) {
  
  if (is.null(inchikey)) {
    stop("Please provide an \"inchikey\".", call. = FALSE)
  }
  
  if (length(inchikey) > 1) {
    stop("Please provide only one \"inchikey\".", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (nchar(inchikey) != 27) {
    warning("Please provide a 27-character \"inchikey\". Not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (length(unlist(strsplit(inchikey, split = "-"))) != 3) {
    warning("Please provide an \"inchikey\" that is hyphen-divided into three parts. Not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14) {
    warning("The first part of the \"inchikey\" should be 14 characters long. Not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10) {
    warning("The second part of the \"inchikey\" should be 10 characters long. Not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1) {
    warning("The third part of the \"inchikey\" should be 1 character long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing query regardless.", call. = FALSE)
  }
  
  curl_data <- list("inchikey" = inchikey)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  curl_url <- "https://api.rsc.org/compounds/v1/tools/validate/inchikey"
  curl_handle <- curl::new_handle()
  curl::handle_setopt(curl_handle, customrequest = "POST", postfields = curl_data)
  curl::handle_setheaders(curl_handle, .list = curl_header)
  result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)
  
  if (result$status_code == 200) {
    TRUE
  } else {
    FALSE
  }
}
