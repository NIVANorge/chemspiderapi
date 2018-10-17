\name{write_image}
\alias{write_image}
\title{Write a .png image}
\description{
Convenience function to write a .png file, e.g., after downloading with \code{chemspider::get_image()}.
}
\usage{
write_image(image, file)
}
\arguments{
\item{image}{Either a base64-encoded character string or a numeric (double) array of the .png image.}
\item{file}{A character string indicating where and under which name to save the image.}
}
\details{
A convenience function to write downloaded .png images, as obtained from \code{chemspider::get_image()}.

If successful, it writes a 150 x 150 px .png image.

If not successful, it returns \code{NA} without writing a file.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## Saving the .png file for Aspirin.

# recordId <- 2157
# apikey <- "a valid 32-character ChemSpider apikey"
# aspirin_img <- get_image(recordId = 2157, apikey = apikey)
# write_image(aspirin_img, "aspirin.png")
}
