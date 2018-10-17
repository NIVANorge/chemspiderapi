\name{get_image}
\alias{get_image}
\title{Get the image file for a compound from ChemSpider}
\description{
This function is used to return the image file from ChemSpider.
}
\usage{
get_image(recordId, apikey, png = FALSE)
}
\arguments{
\item{recordId}{A valid (integer) ChemSpider ID.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
\item{png}{\code{logical}: Should the base64-encoded character string be converted to a (raw) .png image?}
}
\details{
If succesfull, returns either a base64-decoded character vector (\code{png = TRUE}) or a numeric (double) array (\code{png = TRUE}). Either of them can be written to the hard drive using \code{chemspider::write_image()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## GET the .png image file for Aspirin

# recordId <- 2157
# apikey <- "a_valid_ChemSpider_API_key"
# get_image(recordId = 2157, apikey = apikey)
# get_image(recordId = 2157, apikey = apikey, png = TRUE)
}
