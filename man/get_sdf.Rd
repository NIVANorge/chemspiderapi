\name{get_sdf}
\alias{get_sdf}
\title{Get the gzipped .mol/.sdf files for a single compound from ChemSpider}
\description{
This function is used to download a single .mol/.sdf file from ChemSpider after \code{chemspideR::get_status()} returns \code{"Complete"}.
}
\usage{
get_sdf(queryId, apikey)
}
\arguments{
\item{queryId}{A valid 36-character ChemSpider \code{queryId}.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Call this endpoint after \code{chemspideR::get_status()} returns \code{"Complete"}.

If successful, returns a character vector of results from the query, a gzipped .mol/.sdf file.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## GET a gzipped .sdf file.

# queryId <- "a valid 36-character ChemSpider apikey"
# apikey <- "a valid 32-character ChemSpider apikey"
# get_sdf(queryId = queryId, apikey = apikey)
}
