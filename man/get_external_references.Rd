\name{get_external_references}
\alias{get_external_references}
\title{Listing of external references}
\description{
Get a listing of external references for a ChemSpider ID.
}
\usage{
get_external_references(recordId, dataSources = NULL, apikey, source = TRUE,
                        sourceUrl = TRUE, externalId = TRUE, externalUrl = TRUE)
}
\arguments{
\item{recordId}{A valid ChemSpider ID.}
\item{dataSources}{Either a character string, a vector of characters, or a list of characters detailing which \code{dataSources} to look up. If left as is, will return all \code{dataSources}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
\item{source}{\code{logical}: Should the source name be returned (ChemSpider default)?}
\item{sourceUrl}{\code{logical}: Should the source URL be returned (ChemSpider default)?}
\item{externalId}{\code{logical}: Should the external ID be returned (ChemSpider default)?}
\item{externalUrl}{\code{logical}: Should the external URL be returned (ChemSpider default)?}
}
\details{
It is recommended to specify which \code{dataSources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspider::get_datasources()} for a complete list of the >300 \code{dataSources}.

If successful, it returns a data frame with four columns: \code{source}, \code{sourceUrl}, \code{externalId}, and \code{externalUrl}; unless any of them are removed by setting them to \code{FALSE} in the function call. In case only one parameter, e.g. the external ID, is returned, the result is a vector.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## get four external references for aspirin.

# apikey <- "a valid 32-character ChemSpider apikey"
# get_references(recordId = 2157, dataSources = "PubChem", apikey = apikey)
}
