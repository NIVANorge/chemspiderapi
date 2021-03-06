% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RECORDS-post_batch.R
\name{post_batch}
\alias{post_batch}
\title{Post a batch of ChemSpider record IDs}
\usage{
post_batch(
  recordIds,
  fields = "all",
  apikey,
  id = TRUE,
  simplify_formula = FALSE,
  coerce = FALSE
)
}
\arguments{
\item{recordIds}{A vector of integer ChemSpider IDs.}

\item{fields}{A character string indicating which fields to return; see Details.}

\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}

\item{id}{\code{logical}: Should the \code{id} column (i.e., the \code{recordId}) be part of the output? Defaults to \code{TRUE}.}

\item{simplify_formula}{\code{logical}: Should formula strings be simplified? Defaults to \code{FALSE}.}

\item{coerce}{\code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.}
}
\value{
A data frame (if multiple fields are returned), or a vector of adequate type if only one field is required.
}
\description{
Post a batch query of up to 100 ChemSpider record IDs.
}
\details{
"The available fields are: SMILES, Formula, InChI, InChIKey, StdInChI, StdInChIKey, AverageMass, MolecularWeight, MonoisotopicMass, NominalMass, CommonName, ReferenceCount, DataSourceCount, PubMedCount, RSCCount, Mol2D, Mol3D."\cr
\cr
If a \code{recordId} is not found, it is silently dropped.
}
\examples{
\dontrun{
## Post a query for the first 50 ChemSpider entries.
recordIds <- 1:50
apikey <- "a valid 32-character ChemSpider apikey"
post_batch(recordIds = recordIds, apikey = apikey)
}
}
\seealso{
\url{https://developer.rsc.org/compounds-v1/apis/post/records/batch}
}
\author{
Raoul Wolf (\url{https://github.com/RaoulWolf/})
}
