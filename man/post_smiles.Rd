\name{post_mass}
\alias{post_mass}
\title{POST a mass with range}
\description{
Functionality to POST a molecular mass with a range, in order to obtain a \code{queryId} for use in \code{chemspider::get_results()}.
}
\usage{
post_mass(mass, range, dataSources, orderBy, orderDirection, apikey)
}
\arguments{
\item{mass}{A valid (i.e., double) molecular mass (Da or g/mol). Has to be within [1,11000].}
\item{range}{A range for the mass to search on (also in Da or g/mol). Has to be within [0.0001,100].}
\item{dataSources}{Optional: A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{orderBy}{Optional: A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{orderDirection}{Optional: A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
If successful, performs the desired conversion and stores the result as a named ("output") character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## convert the InChI string of Aspirin to SMILES.

# apikey <- "a_valid_ChemSpider_API_key"
# post_convert(input = "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)",
#              inputFormat = "InChI",
#              outputFormat = "SMILES",
#              apikey = apikey)
}
