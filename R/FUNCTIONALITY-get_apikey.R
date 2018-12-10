#' Retrieve a ChemSpider API Key
#' 
#' Functionality to retrieve a ChemSpider API key which has been previously stored.
#' 
#' This function is useful to access your ChemSpider API key.\cr
#' \cr
#' To save the ChemSpider API key, the function \code{chemspiderapi::save_apikey()} is provided.
#' 
#' @param service The name of the key ring service. Defaults to "ChemSpider API Key".
#' @param username The username. Defaults to the username as returned by \code{Sys.info()}.
#' @return Securely return the ChemSpider API key as character string.
#' @examples 
#' \dontrun{
#' ## get an API key
#' get_apikey()
#' }
#' @export
get_apikey <- function(service = "ChemSpider API Key", username = NULL) {
  
  if (is.null(username)) {
    username <- unname(Sys.info()["user"])
  }
  
  keyring::key_get(service = service, username = username)
}