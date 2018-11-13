#' Set a rate limit for chemspiderapi
#' 
#' Functionality to set a rate limit for (nearly) all chemspiderapi functions
#' 
#' This function is useful if you have rate limitations from ChemSpider.\cr
#' \cr
#' To "reset" the functions to a non-rate limited function, use \code{chemspiderapi::remove_rate_limit()}.
#' 
#' @param rate (Integer) number of calls to make in a specific time limit.
#' @param limit (Double) Time range over which to apply the limit, in seconds.
#' @return Creates rate-limited functions of (most) chemspiderapi functions
#' @examples 
#' \dontrun{
#' ## Set a rate limit for all chemspiderapi functions
#' rate <- 15
#' limit <- 60
#' set_rate_limit(rate = rate, limit = limit)
#' }
#' @export
set_rate_limit <- function(rate, limit) {
  
  if (is.na(rate)) {
    warning("No \"rate\" provided.", call. = FALSE)
    stop(call. = FALSE)
  }
  
  if (is.na(as.integer(rate))) {
    warning("No numeric \"rate\" provided.", call. = FALSE)
    stop(call. = FALSE)
  }
  
  if (length(rate) > 0) {
    warning("Only one single \"rate\" is supported.", call. = FALSE)
    stop(call. = FALSE)
  }
  
  if (is.na(limit)) {
    warning("No \"limit\" provided.", call. = FALSE)
    stop(call. = FALSE)
  }
  
  if (is.na(as.double(limit))) {
    warning("No numeric \"limit\" provided.", call. = FALSE)
    stop(call. = FALSE)
  }
  
  if (length(limit) > 0) {
    warning("Only one single \"limit\" is supported.", call. = FALSE)
    stop(call. = FALSE)
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
