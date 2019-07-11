#' Validate an InChIKey
#' 
#' Functionality to check the validity of an InChIKey.
#' 
#' If successful (i.e., the InChIKey is valid), returns \code{TRUE}.\cr
#' \cr
#' If not successful (i.e., the InChIKey is not valid), returns \code{FALSE}.\cr
#' \cr
#' Before this functions performs an API query, it runs quality checking as lined out at \url{https://www.inchi-trust.org/technical-faq/#13.1}. If an InChIKey is ruled out based on these criteria, it returns \code{FALSE} with a warning message.
#' 
#' @param inchikey A 27-character InChIKey to be validated.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A logical indicating the validity of the POSTed InChIKey
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/validate/inchikey}
#' @examples
#' \dontrun{
#' ## validate the InChIKey of aspirin
#' inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#' apikey <- "a_valid_ChemSpider_API_key"
#' post_validate_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @export
post_validate_inchikey <- function(inchikey, apikey) {
  
  if (is.na(inchikey)) {
    stop("Please provide an \"inchikey\". Not performing API query.", call. = FALSE)
  }
  
  if (length(inchikey) > 1) {
    stop("Please provide only one \"inchikey\". Not performing API query.", call. = FALSE)
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
    warning("The first part of the \"inchikey\" should be 14 characters long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10) {
    warning("The first part of the \"inchikey\" should be 10 characters long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1) {
    warning("The third part of the \"inchikey\" should be 1 character long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA)
  }
  
  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing query regardless.", call. = FALSE)
  }
  
  curlData <- list(inchikey = inchikey)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/tools/validate/inchikey"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST", postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  
  if (result$status_code == 200) {
    TRUE
  }
  
  else {
    FALSE
  }
}
