\name{post_formula}
\alias{post_formula}
\title{POST a chemical formula}
\description{
Functionality to POST a chemical formula, in order to obtain a \code{queryId} for use in \code{chemspider::get_results()}.
}
\usage{
post_formula(formula, dataSources = "", orderBy = "", orderDirection = "", apikey)
}
\arguments{
\item{formula}{A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{dataSources}{A character string indicating which format the input has. }
\item{orderBy}{A character string indicating which type of output is desired.}
\item{orderDirection}{A character string indicating which type of output is desired.}
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
