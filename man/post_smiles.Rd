\name{post_smiles}
\alias{post_smiles}
\title{POST a SMILES string}
\description{
Functionality to POST a SMILES string to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_smiles(smiles, apikey)
}
\arguments{
\item{smiles}{A SMILES string.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
If successful, \code{chemspideR::post_smiles()} returns the \code{queryId} as character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST the SMILES string of Aspirin to get a queryId.

# smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"
# apikey <- "a valid 32-character ChemSpider apikey"
# post_smiles(smiles = smiles, apikey = apikey)
}
