#' Save a ChemSpider API Key
#' 
#' Functionality to interactively set a ChemSpider API key via keyring.
#' 
#' This function is useful to effectively and securely store your ChemSpider API key. The API is entered interactively in a pop-up console.\cr
#' \cr
#' To retrieve the ChemSpider API key, the function \code{chemspiderapi::get_apikey()} is provided.
#' 
#' @param service The name of the key ring service. Defaults to "ChemSpider API Key".
#' @param username The username. Defaults to the username as returned by \code{base::Sys.info()}.
#' @return Stores the ChemSpider API key as system key.
#' @examples 
#' \dontrun{
#' ## save an API key
#' save_apikey()
#' }
#' @export
save_apikey <- function(service = "ChemSpider API Key", username = NULL) {
  
  if (is.null(username)) {
    username <- unname(Sys.info()["user"])
  }
  
  keyring::key_set(service = service, username = username)
}
