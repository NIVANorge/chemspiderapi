\name{write_mol}
\alias{write_mol}
\title{Write a .mol file}
\description{
Convenience function to write a .mol file, e.g., after downloading with \code{chemspider::get_mol()}.
}
\usage{
write_mol(mol, file)
}
\arguments{
\item{mol}{A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{file}{A character string indicating where and under which name to save the .mol file.}
}
\details{
A pure convenience function to write .mol files. Relies on \code{base::writeLines()}.

If successful, it writes a .mol file.

If not successful, it returns \code{NA} without writing a file.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Saving the .mol file for Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
# aspirin_mol <- get_mol(recordId = 2157,
#                        apikey = apikey)
# write_mol(aspirin_mol, "aspirin.mol")
}
