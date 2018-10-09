\name{get_mol}
\alias{get_mol}
\title{Get the .mol/.sdf file for a compound from ChemSpider}
\description{
This function is used to return the .mol/.sdf file from ChemSpider.
}
\usage{
get_mol(recordId, apikey)
}
\arguments{
\item{recordId}{A valid (integer) ChemSpider Record ID. This is sometimes simply referred to as ChemSpider ID.}
\item{apikey}{A 32 character string with a valid key for ChemSpider's API services.}
}
\details{
Call this endpoint with \code{recordId} as an integer.

If successful, returns a character vector of results from the query, a .mol/.sdf file. This character vector can be written to the hard drive using \code{chemspider::write_mol()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## .mol/.sdf file for Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
#
# get_mol(recordId = 2157, apikey = apikey)
}
