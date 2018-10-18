\name{post_name}
\alias{post_name}
\title{POST a name to obtain a ChemSpider query ID}
\description{
POST the name of a compound and obtain a ChemSpider query ID for subsequent use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_name(name, orderBy = "recordId", orderDirection = "ascending", apikey)
}
\arguments{
\item{name}{A character string of the compound name.}
\item{orderBy}{A character string indicating by which parameter the results should be arranged (NOT case sensitive); see Details.}
\item{orderDirection}{A character string indicating which in which direction the results should be arranged (NOT case sensitive); see Details.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Allowed entries for \code{orderBy} are:

\code{"recordId"} (default)

\code{"massDefect"}

\code{"molecularWeight"}

\code{"referenceCount"}

\code{"dataSourceCount"}

\code{"pubMedCount"}

\code{"rscCount"}

Allowed entries for \code{orderDirection} are:

\code{"ascending"} (default)

\code{"descending"}

If successful, returns a 36-character \code{queryId} as a named character string. The \code{queryId} can be used in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST the name of Aspirin to obtain a query ID

# name <- "Aspirin"
# apikey <- "a valid 32-character ChemSpider apikey"
# post_name(name = name, apikey = apikey)
}
