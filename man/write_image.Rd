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
\item{image}{A valid image, either base64-encoded or already converted to a \code{magick-image}.}
\item{file}{A character string indicating where and under which name to save the image.}
}
\details{
A convenience function to write downloaded .png images (from \code{chemspider::get_image()}).

If successful, it writes a 150 x 150 px .png image ().

If not successful, it returns \code{NA} without writing a file.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Saving the .png file for Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
# aspirin_img <- get_image(recordId = 2157,
#                          apikey = apikey,
#                          magic = TRUE)
# write_image(aspirin_img, "aspirin.png")
}
