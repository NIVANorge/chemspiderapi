\name{post_inchi}
\alias{post_inchi}
\title{POST an InChI string}
\description{
Functionality to POST an InChI string to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_inchi(inchi, apikey)
}
\arguments{
\item{inchi}{A valid InChI string; see Details.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
The validity criteria for InChI stringss are outlined here: https://www.inchi-trust.org/technical-faq/#2.8. If certain criteria are not met by the input \code{inchi}, \code{chemspideR::post_inchi()} returns a warning message and \code{NA}, and does not perform an API query.

If successful, returns the \code{queryId} as character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST the InChI string of Aspirin to get a queryId.

# inchi <- "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
# apikey <- "a valid 32-character ChemSpider apikey"
# post_inchi(inchi = inchi, apikey = apikey)
}
