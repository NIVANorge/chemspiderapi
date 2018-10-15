\name{post_batch}
\alias{post_batch}
\title{Get results for batches}
\description{
Get ChemSpider details for up to 100 compounds at once.
}
\usage{
post_batch(recordIds, fields = "all", apikey, id = TRUE)
}
\arguments{
\item{recordIds}{A valid .mol file, e.g., as downloaded via \code{chemspideR::get_mol()}.}
\item{fields}{A character string indicating which fields to return. See \code{chemspideR::get_details()} for a description of possible fields}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
\item{id}{\code{logical}: Should the recordId be returned (ChemSpider default)?}
}
\details{

Note: if a ChemSpider ID ("recordId") is not found, it is silently dropped!

If successful, performs the desired conversion and stores the result as a named ("output") character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## get details for the first 50 ChemSpider entries.

# apikey <- "a_valid_ChemSpider_API_key"
# post_batch(recordIds = 1:50, fields = "all", apikey = apikey)
}
