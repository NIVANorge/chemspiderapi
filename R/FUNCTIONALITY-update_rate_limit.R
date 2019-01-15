#' Update a Rate Limit for chemspiderapi Functions
#' 
#' Functionality to update a rate limit for all chemspiderapi functions interacting with ChemSpider's API services on the fly.
#' 
#' This function is useful if you have to update rate limitations from ChemSpider.\cr
#' \cr
#' To "reset" the functions to a non-rate limited function, use \code{chemspiderapi::remove_rate_limit()}.
#' 
#' @param rate Number (integer) of calls to make within a specific time limit.
#' @param limit Time range (integer) over which to apply the limit, in seconds.
#' @return Creates updated rate-limited functions of all chemspiderapi functions interacting with ChemSpider's API functions.
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
