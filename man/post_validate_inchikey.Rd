\name{post_validate_inchikey}
\alias{post_validate_inchikey}
\title{Validate an InChIKey}
\description{
Functionality to check wether an InChIKey is valid.
}
\usage{
post_validate_inchikey(inchikey, apikey)
}
\arguments{
\item{inchikey}{An InChIKey to be validated.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Submit an (potential) InChIKey.

If successful (i.e., the InChIKey is valid), it returns \code{TRUE}.

If not successful (i.e., the InChIKey is not valid), it returns \code{FALSE}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## validate the InChIKey of Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
# post_validate_inchikey(inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", apikey = apikey)
}
