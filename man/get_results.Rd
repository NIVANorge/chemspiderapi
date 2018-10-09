\name{get_results}
\alias{get_results}
\title{Get results from queries}
\description{
Get results from queries.
}
\usage{
get_results(queryId, start = "", count = "", apikey)
}
\arguments{
\item{queryId}{A valid query ID, e.g., as returned by \code{chemspider::post_smiles()}.}
\item{start}{An integer value giving the position from which to start the retrival of query results. See Details.}
\item{count}{An integer value giving the the number of query results to retrieve. See Details.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
If the results have been truncated because there were more results than the maximum number of results permitted (by default, 10,000), a warning is issued. If this happens, you can split your requests into smaller batches.

To batch your requests, call this function with two optional parameters, \code{start} and \code{count}. Both take integer values. \code{start} is the number of the record to start with (zero-based), and \code{count} is the number of records to return. For example, to request results 200-300, use \code{start = 200} and \code{count = 100}.

If successful, returns a ChemSpider ID (i.e., \code{recordId}).

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Obtain the result from a random query.

# apikey <- "a_valid_ChemSpider_API_key"
# queryId <- "a_valid_ChemSpider_query_ID"
#
# get_results(queryId = queryId, apikey = apikey)
}
