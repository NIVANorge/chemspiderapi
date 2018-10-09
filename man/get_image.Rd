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
\item{recordId}{a valid (integer) ChemSpider Record ID. This is sometimes simply referred to as ChemSpider ID.}
\item{apikey}{a 32 character string with a valid key for ChemSpider's API services.}
\item{png}{\code{logical}: Should the base64-encoded character string be converted to a (raw) .png image?}
}
\details{
Call this endpoint with \code{recordId} as an integer.

Do not use your \code{apikey} as plain text. See example below, although there are likely safer options.

If successful, returns a image results from the query. Depending on the specifications, this is either a character string (\code{png = FALSE}) or a numeric array (\code{magick = png}). The numeric array can be written to the hard drive using \code{chemspider::write_image()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## image file for Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
#
# get_image(recordId = 2157, apikey = apikey, magick = TRUE)
}
