\name{get_details}
\alias{get_details}
\title{Get the record details for a compound from ChemSpider}
\description{
This function is used to return record details from ChemSpider.
}
\usage{
get_details(recordId, fields, apikey, id = TRUE)
}
\arguments{
\item{recordId}{a valid (integer) ChemSpider Record ID. This is sometimes simply referred to as ChemSpider ID.}
\item{fields}{either a single character string, a character vector, or a character list stating which fields to return. Alternatively, \code{"all"} returns all possible \code{fields}. \code{fields} is NOT case sensitive, but see details for a list of possible entries.}
\item{apikey}{a 32 character string with a valid key for ChemSpider's API services.}
\item{id}{\code{logical}: should the \code{id} column (i.e., the \code{recordId}) be part of the output (ChemSpider default)?}
}
\details{
Call this endpoint with \code{recordId} as an integer.

The available inputs for \code{fields} are:

\code{SMILES}

\code{Formula}

\code{AverageMass}

\code{MolecularWeight}

\code{MonoisotopicMass}

\code{NominalMass}

\code{CommonName}

\code{ReferenceCount}

\code{DataSourceCount}

\code{PubMedCount}

\code{RSCCount}

\code{Mol2D}

\code{Mol3D}

If successful, returns a \code{data.frame} of results from the query, with the requested fields.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Record details for Aspirin.

# apikey <- "a_valid_ChemSpider_API_key"
#
# get_details(recordId = 2157, fields = c("SMILES", "MolecularWeight", "CommonName"),
#             apikey = apikey)
}
