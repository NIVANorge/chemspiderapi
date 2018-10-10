\name{post_smiles}
\alias{post_smiles}
\title{POST a SMILES string}
\description{
Functionality to POST a SMILES string, in order to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_smiles(smiles, apikey)
}
\arguments{
\item{smiles}{A valid SMILES character string.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Post the SMILES character string of aspirin.

# smiles <- "CC(=O)Oc1ccccc1C(=O)O"
# apikey <- "a_valid_ChemSpider_API_key"
#
# post_smiles(smiles = smiles, apikey = apikey)
}
