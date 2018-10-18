\name{get_formula_batch_results}
\alias{get_formula_batch_results}
\title{GET the results from a ChemSpider query}
\description{
GET the results from a ChemSpider query after \code{chemspideR::get_formula_batch_status()} returns \code{"Complete"}.
}
\usage{
get_formula_batch_results(queryId, apikey)
}
\arguments{
\item{queryId}{A valid 36-character query ID, as returned by \code{chemspider::post_formula_batch()}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Before running \code{chemspideR::get_formula_batch_results()}, make sure \code{chemspideR::get_formula_batch_status()} returns \code{"Complete"}!

If successful, returns a data frame with the query results; in case the response is a single value, e.g., a ChemSpider ID, it is returned as single vector.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## Obtain the result from a mass batch query.

# apikey <- "a valid 32-character ChemSpider apikey"
# queryId <- "a valid 36-character ChemSpider queryId"
#
# get_formula_batch_results(queryId = queryId, apikey = apikey)
}
