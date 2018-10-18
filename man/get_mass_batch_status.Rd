\name{get_mass_batch_status}
\alias{get_mass_batch_status}
\title{GET the status for a mass batch query from ChemSpider}
\description{
This function is used to return the status of a query from \code{chemspideR::post_mass_batch()}.
}
\usage{
get_mass_batch_status(queryId, count = TRUE, message = TRUE, apikey)
}
\arguments{
\item{queryId}{A valid 36-character ChemSpider query ID obtained from \code{chemspideR::post_mass_batch()}.}
\item{count}{\code{logical}: Should the count of the results be returned (ChemSpider default)?}
\item{message}{\code{logical}: Should the message be returned (ChemSpider default)?}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Call this endpoint with a \code{queryId} obtained from \code{chemspideR::post_mass_batch()}.

If the query is still ongoing, returns a warning and a character vector of the query status as \code{Incomplete}. It is recommended to wait at least ten seconds before checking the status again.

If the query is finalized, returns a data frame of the query status with \code{status}, \code{count} and \code{message}. The \code{status} can be either \code{Complete}, \code{Suspended}, \code{Failed}, or \code{Not Found}.

Says ChemSpider:

"A status of Suspended can be caused if the results could not be compiled within a reasonable amount of time. Create a new filter request with more restrictive parameters.

A status of Failed can be caused if the backend system could not compile the results. Create a new filter request and, if the same outcome occurs, apply more restrictive parameters.

A status of Not Found can be caused if the Query ID has not been registered or has expired. Create a new filter request."

If both \code{count} and \code{message} are set to \code{FALSE}, \code{chemspideR::get_status()} returns the \code{status} as character vector.

If the status is \code{Complete}, the results of the query can be obtained from \code{chemspideR::get_results()}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## Get the status of a mass batch query from ChemSpider

# queryId <- "a valid 36-character ChemSpider queryId"
# apikey <- "a valid 32-character ChemSpider apikey"
# get_mass_batch_status(queryId = queryId, apikey = apikey)
}
