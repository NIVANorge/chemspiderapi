\name{get_details}
\alias{get_details}
\title{GET the record details for a compound from ChemSpider}
\description{
This function is used to return record details from ChemSpider.
}
\usage{
get_details(recordId, fields = "all", apikey, id = TRUE)
}
\arguments{
\item{recordId}{A valid (integer) ChemSpider ID.}
\item{fields}{Either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
\item{id}{\code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output (ChemSpider default)?}
}
\details{
The available options for \code{fields} are:

\code{"SMILES"}

\code{"Formula"}

\code{"AverageMass"}

\code{"MolecularWeight"}

\code{"MonoisotopicMass"}

\code{"NominalMass"}

\code{"CommonName"}

\code{"ReferenceCount"}

\code{"DataSourceCount"}

\code{"PubMedCount"}

\code{"RSCCount"}

\code{"Mol2D"}

\code{"Mol3D"}

\code{"all"} (default)

If successful, returns either a \code{data.frame} of results from the query with the requested fields; if only one paraemeter is returned, this is then transformed into a vector.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## Record details for Aspirin.

# recordId <- 2157
# apikey <- "a_valid_ChemSpider_API_key"
# get_details(recordId = recordId, fields = c("SMILES", "MolecularWeight", "CommonName"),
#             apikey = apikey)
# get_details(recordId = recordId, fields = "MolecularWeight", apikey = apikey,
#             id = FALSE)
}
