\name{get_mol}
\alias{get_mol}
\title{Get the .mol/.sdf file for a single compound from ChemSpider}
\description{
This function is used to download a single .mol/.sdf file from ChemSpider.
}
\usage{
get_mol(recordId, apikey)
}
\arguments{
\item{recordId}{A valid (integer) ChemSpider ID.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Call this endpoint with \code{recordId} as an integer.

If successful, returns a character vector of results from the query, a .mol/.sdf file. This character vector can be written to the hard drive using \code{chemspider::write_mol()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## GET the .mol/.sdf file for Aspirin.

# recordId <- 2157
# apikey <- "a valid 32-character ChemSpider apikey"
# get_mol(recordId = recordId, apikey = apikey)
}
