\name{get_results}
\alias{get_results}
\title{GET the results from a ChemSpider query}
\description{
GET the results from a ChemSpider query after \code{chemspideR::get_status()} returns \code{"Complete"}.
}
\usage{
get_results(queryId, start = NULL, count = NULL, apikey)
}
\arguments{
\item{queryId}{A valid 36-character query ID, e.g., as returned by \code{chemspider::post_smiles()}.}
\item{start}{Optional: An integer value giving the position from which to start the retrival of query results. See Details.}
\item{count}{Optional: An integer value giving the the number of query results to retrieve. See Details.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Before running \code{chemspideR::get_results()}, make sure \code{chemspideR::get_status()} returns \code{"Complete"}!

If the results have been truncated because there were more results than the maximum number of results permitted (by default, 10,000), a warning is issued. If this happens, you can split your requests into smaller batches.

To batch your requests, call this function with two optional parameters, \code{start} and \code{count}. Both take integer values. \code{start} is the number of the record to start with (zero-based), and \code{count} is the number of records to return. For example, to request results 200-300, use \code{start = 200L} and \code{count = 100L}.

If successful, returns a data frame with the query results; in case the response is a single value, e.g., a ChemSpider ID, it is returned as single vector.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## Obtain the result from a query.

# apikey <- "a valid 32-character ChemSpider apikey"
# queryId <- "a valid 36-character ChemSpider queryId"
#
# get_results(queryId = queryId, apikey = apikey)
# get_results(queryId = queryId, start = 200L, count = 100L, apikey = apikey)
}
