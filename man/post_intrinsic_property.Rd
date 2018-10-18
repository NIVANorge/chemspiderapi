\name{post_intrinsic_property}
\alias{post_intrinsic_property}
\title{POST an intrinsic property}
\description{
Functionality to POST an intrinsic property to obtain a \code{queryId} for use in \code{chemspider::get_status()} and \code{chemspider::get_results()}.
}
\usage{
post_intrinsic_property(property, formula = NULL, complexity = "any", isotopic = "any",
                        mass = NULL, range = NULL, orderBy = "recordId",
                        orderDirection = "ascending", apikey)
}
\arguments{
\item{property}{See Details.}
\item{dataSources}{Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspideR::get_data_sources()}}
\item{orderBy}{A character string indicating by which parameter the results should be ordered; see Details.}
\item{orderDirection}{A character string indicating in which direction the results should be ordered; see Details.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Valid entries for property are: formula, molecularWeight, nominalMass, averageMass, or monoisotopicMass

Says ChemSpider:

"Valid values for \code{complexity} are \code{"any"}, \code{"single"}, or \code{"multiple"} whereby a compound with a complexity of multiple has more than one disconnected system in it or a metal atom or ion.

Valid values for \code{isotopic} are \code{"any"}, \code{"labeled"}, or \code{"unlabeled"}.

You can use \code{orderBy} and \code{orderDirection} to specify the sort order for the results. Valid values for \code{orderBy} are \code{"recordId"}, \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"}, \code{"rscCount"}.

Valid values for \code{orderDirection} are \code{"ascending"}, \code{"descending"}."

If successful, returns the \code{queryId} as character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST an intrinsic property for Aspirin to get a queryId.

# apikey <- "a valid 32-character ChemSpider apikey"
# post_intrinsic_property(property = "Formula", formula = "C9H8O4", apikey = apikey)
# post_intrinsic_property(property = "MolecularWeight", mass = 180, range = 0.5, apikey = apikey)
}
