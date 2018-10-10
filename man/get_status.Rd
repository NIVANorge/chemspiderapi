\name{get_status}
\alias{get_status}
\title{Get the status for a query from ChemSpider}
\description{
This function is used to return the status of a query from ChemSpider.
}
\usage{
get_status(queryId, apikey)
}
\arguments{
\item{queryId}{A valid ChemSpider query ID. This is usually returned by POST functions, e.g., \code{chemspideR::post_inchikey()}}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Call this endpoint with a \code{queryId} obtained from a previous POST query (e.g., \code{chemspideR::post_inchikey()}).

If the query is still ongoing, returns a warning and a character vector of the query status as \code{Incomplete}. It is recommended to wait at least 10 seconds before checking the status again.

If the query is finalized, returns a character vector of the query status, i.e., either \code{Complete}, \code{Suspended}, \code{Failed}, or \code{Not Found}. If the status is \code{Complete}, the results of the query can be obtained from \code{chemspideR::get_results()}.

Says ChemSpider:

"A status of Suspended can be caused if the results could not be compiled within a reasonable amount of time. Create a new filter request with more restrictive parameters.

A status of Failed can be caused if the backend system could not compile the results. Create a new filter request and, if the same outcome occurs, apply more restrictive parameters.

A status of Not Found can be caused if the Query ID has not been registered or has expired. Create a new filter request."

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Get the status of a query from ChemSpider

# queryId <- "a_valid_queryId"
# apikey <- "a_valid_ChemSpider_API_key"
#
# get_status(queryId = queryId, apikey = apikey)
}
