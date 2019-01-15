#' Remove a Rate Limit for chemspiderapi Functions
#' 
#' Functionality to remove all rate limits for chemspiderapi functions interacting with ChemSpider's API services.
#' 
#' This function is useful to remove any rate limitations from chemspiderapi functionalities, as provided by \code{chemspiderapi::set_rate_limit()}.
#' 
#' @return Removes any rate-limition of chemspiderapi functions.
#' @examples 
#' \dontrun{
#' ## Set a rate limit for all chemspiderapi functions
#' rate <- 15
#' limit <- 60
#' set_rate_limit(rate = rate, limit = limit)
#' 
#' ## Remove the rate limit
#' remove_rate_limit()
#' }
#' @export
remove_rate_limit <- function() {
  
  post_batch <- chemspiderapi::post_batch
  post_convert <- chemspiderapi::post_convert
  post_element <- chemspiderapi::post_element
  post_formula <- chemspiderapi::post_formula
  post_formula_batch <- chemspiderapi::post_formula_batch
  post_inchi <- chemspiderapi::post_inchi
  post_inchikey <- chemspiderapi::post_inchikey
  post_intrinsicproperty <- chemspiderapi::post_intrinsicproperty
  post_mass <- chemspiderapi::post_mass
  post_mass_batch <- chemspiderapi::post_mass_batch
  post_name <- chemspiderapi::post_name
  post_smiles <- chemspiderapi::post_smiles
  post_validate_inchikey <- chemspiderapi::post_validate_inchikey
  
}
