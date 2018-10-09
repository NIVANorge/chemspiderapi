\name{get_external_references}
\alias{get_external_references}
\title{Listing of external references}
\description{
Get a listing of external references for a ChemSpider ID.
}
\usage{
get_external_references(recordId, dataSources = "", apikey)
}
\arguments{
\item{recordId}{A valid ChemSpider ID.}
\item{dataSources}{Either a character string, a vector of characters, or a list of characters detailing which \code{dataSources} to look up. If left as is, will return all \code{dataSources}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
It is recommended to specify which \code{dataSources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspider::get_datasources()} for a complete list of the >300 \code{dataSources}.

If a data source is not found, it is automatically dropped and a warning is issued.

If successful, it returns a data frame with four columns: \code{source}, \code{sourceUrl}, \code{externalId}, and \code{externalUrl}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## get four external references for aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
#
# get_references(recordId = 2157, dataSources = c("ChEMBL", "KEGG", "NIST", "PubChem"),
#                apikey = apikey)
}
