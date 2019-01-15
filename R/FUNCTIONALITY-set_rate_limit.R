#' Set a Rate Limit for chemspiderapi Functions
#' 
#' Functionality to set a rate limit for all chemspiderapi functions interacting with ChemSpider's API services.
#' 
#' This function is useful if you have rate limitations from ChemSpider.\cr
#' \cr
#' To "reset" the functions to a non-rate limited function, use \code{chemspiderapi::remove_rate_limit()}.
#' 
#' @param rate Number (integer) of calls to make within a specific time limit.
#' @param limit Time range (integer) over which to apply the limit, in seconds.
#' @return Creates rate-limited functions of all chemspiderapi functions interacting with ChemSpider's API services.
#' @examples 
#' \dontrun{
#' ## Set a rate limit for all chemspiderapi functions
#' rate <- 15
#' limit <- 60
#' set_rate_limit(rate = rate, limit = limit)
#' }
#' @export
set_rate_limit <- function(rate, limit) {
  
  if (is.na(as.integer(rate))) {
    stop("Please provide a numeric (integer) \"rate\".", call. = FALSE)
  }
  
  if (length(rate) > 1) {
    stop("Please provide only one \"rate\".", call. = FALSE)
  }
  
  if (is.double(rate)) {
    rate <- as.integer(rate)
  }
  
  if (is.na(as.integer(limit))) {
    stop("Please provide a numeric (integer) \"limit\".", call. = FALSE)
  }
  
  if (length(limit) > 1) {
    stop("Please provide only one \"limit\".", call. = FALSE)
  }
  
  if (is.double(limit)) {
    rate <- as.integer(limit)
  }
  
  post_batch <- ratelimitr::limit_rate(chemspiderapi::post_batch, ratelimitr::rate(n = rate, period = limit))
  post_convert <- ratelimitr::limit_rate(chemspiderapi::post_convert, ratelimitr::rate(n = rate, period = limit))
  post_element <- ratelimitr::limit_rate(chemspiderapi::post_element, ratelimitr::rate(n = rate, period = limit))
  post_formula <- ratelimitr::limit_rate(chemspiderapi::post_formula, ratelimitr::rate(n = rate, period = limit))
  post_formula_batch <- ratelimitr::limit_rate(chemspiderapi::post_formula_batch, ratelimitr::rate(n = rate, period = limit))
  post_inchi <- ratelimitr::limit_rate(chemspiderapi::post_inchi, ratelimitr::rate(n = rate, period = limit))
  post_inchikey <- ratelimitr::limit_rate(chemspiderapi::post_inchikey, ratelimitr::rate(n = rate, period = limit))
  post_intrinsicproperty <- ratelimitr::limit_rate(chemspiderapi::post_intrinsicproperty, ratelimitr::rate(n = rate, period = limit))
  post_mass <- ratelimitr::limit_rate(chemspiderapi::post_mass, ratelimitr::rate(n = rate, period = limit))
  post_mass_batch <- ratelimitr::limit_rate(chemspiderapi::post_mass_batch, ratelimitr::rate(n = rate, period = limit))
  post_name <- ratelimitr::limit_rate(chemspiderapi::post_name, ratelimitr::rate(n = rate, period = limit))
  post_smiles <- ratelimitr::limit_rate(chemspiderapi::post_smiles, ratelimitr::rate(n = rate, period = limit))
  post_validate_inchikey <- ratelimitr::limit_rate(chemspiderapi::post_validate_inchikey, ratelimitr::rate(n = rate, period = limit))
  
}
