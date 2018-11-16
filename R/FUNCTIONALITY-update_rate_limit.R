#' Update a rate limit for chemspiderapi
#' 
#' Functionality to update a rate limit for (nearly) all chemspiderapi functions on the fly.
#' 
#' This function is useful if you have to update rate limitations from ChemSpider.\cr
#' \cr
#' To "reset" the functions to a non-rate limited function, use \code{chemspiderapi::remove_rate_limit()}.
#' 
#' @param rate (Integer) number of calls to make in a specific time limit.
#' @param limit (Double) Time range over which to apply the limit, in seconds.
#' @return Creates an updated rate-limited functions of (most) chemspiderapi functions
#' @examples 
#' \dontrun{
#' ## Set a rate limit for all chemspiderapi functions
#' rate1 <- 15
#' limit1 <- 60
#' set_rate_limit(rate = rate1, limit = limit1)
#' 
#' ## Update a rate limit for all chemspiderapi functions
#' rate2 <- 500
#' limit2 <- 86400
#' update_rate_limit(rate = rate2, limit = limit2)
#' }
#' @export
update_rate_limit <- function(rate, limit) {
  
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
  
  post_batch <- ratelimitr::UPDATE_RATE(post_batch, ratelimitr::rate(n = rate, period = limit))
  
  post_convert <- ratelimitr::UPDATE_RATE(post_convert, ratelimitr::rate(n = rate, period = limit))
  
  post_element <- ratelimitr::UPDATE_RATE(post_element, ratelimitr::rate(n = rate, period = limit))
  
  post_formula <- ratelimitr::UPDATE_RATE(post_formula, ratelimitr::rate(n = rate, period = limit))
  
  post_formula_batch <- ratelimitr::UPDATE_RATE(post_formula_batch, ratelimitr::rate(n = rate, period = limit))
  
  post_inchi <- ratelimitr::UPDATE_RATE(post_inchi, ratelimitr::rate(n = rate, period = limit))
  
  post_inchikey <- ratelimitr::UPDATE_RATE(post_inchikey, ratelimitr::rate(n = rate, period = limit))
  
  post_intrinsicproperty <- ratelimitr::UPDATE_RATE(post_intrinsicproperty, ratelimitr::rate(n = rate, period = limit))
  
  post_mass <- ratelimitr::UPDATE_RATE(post_mass, ratelimitr::rate(n = rate, period = limit))
  
  post_mass_batch <- ratelimitr::UPDATE_RATE(post_mass_batch, ratelimitr::rate(n = rate, period = limit))
  
  post_name <- ratelimitr::UPDATE_RATE(post_name, ratelimitr::rate(n = rate, period = limit))
  
  post_smiles <- ratelimitr::UPDATE_RATE(post_smiles, ratelimitr::rate(n = rate, period = limit))
  
  post_validate_inchikey <- ratelimitr::UPDATE_RATE(post_validate_inchikey, ratelimitr::rate(n = rate, period = limit))
  
}