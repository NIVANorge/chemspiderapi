\name{post_batch}
\alias{post_batch}
\title{Get results for batches}
\description{
Get ChemSpider details for up to 100 compounds at once.
}
\usage{
post_batch(recordIds, fields = "all", apikey, id = TRUE)
}
\arguments{
\item{recordIds}{A vector of integer ChemSpider IDs.}
\item{fields}{A character string indicating which fields to return; see Details.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
\item{id}{\code{logical}: Should the recordId be returned (ChemSpider default)?}
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

If a \code{recordId} is not found, it is silently dropped.

If successful, returns a data frame with each row being a \code{recordId} and each column being one of \code{fields}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## get details for the first 50 ChemSpider entries.

# recordIds <- 1:50
# apikey <- "a valid 32-character ChemSpider apikey"
# post_batch(recordIds = recordIds, apikey = apikey)
}
