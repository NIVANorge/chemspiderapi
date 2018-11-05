\name{post_inchikey}
\alias{post_inchikey}
\title{POST an InChIKey as ChemSpider query}
\description{
Functionality to POST an InChIKey to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_inchikey(inchikey, apikey)
}
\arguments{
\item{inchikey}{A valid 27-character InChIKey; see Details.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
The validity criteria for InChIKeys are outlined here: https://www.inchi-trust.org/technical-faq/#13.1. If certain criteria are not met by the input \code{inchikey}, \code{chemspideR::post_inchikey()} returns a warning message and \code{NA} (and does not perform an API query). In the case of a non-standard \code{inchikey}, a warning is issued but the query will be performed.

If successful, returns the \code{queryId} as character string.

If not successful, returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
}
\examples{
## Do not run
## POST the InChIKey of Aspirin to get a queryId.

# inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
# apikey <- "a valid 32-character ChemSpider apikey"
# post_inchikey(inchikey = inchikey, apikey = apikey)
}
