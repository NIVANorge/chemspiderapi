#' Remove a ChemSpider API Key
#' 
#' Functionality to remove a previously stored ChemSpider API key.
#' 
#' This function is useful to remove an existing ChemSpider API key, for example because of a typo in the API key.
#' 
#' @param service The name of the key ring service. Defaults to "ChemSpider API Key".
#' @param username The username. Defaults to the username as returned by \code{base::Sys.info()}.
#' @return Securely removes the ChemSpider API key.
#' @examples 
#' \dontrun{
#' ## get an API key
#' remove_apikey()
#' }
#' @export
remove_apikey <- function(service = "ChemSpider API Key", username = NULL) {
  
  if (is.null(username)) {
    username <- unname(Sys.info()["user"])
  }
  
  keyring::key_delete(service = service, username = username)
}