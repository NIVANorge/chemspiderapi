\name{post_formula}
\alias{post_formula}
\title{POST a formula}
\description{
Functionality to POST a formula to obtain a \code{queryId} for use in \code{chemspider::get_status()} and \code{chemspider::get_results()}.
}
\usage{
post_formula(formula, dataSources = NULL, orderBy = "recordId",
             orderDirection = "ascending", apikey)
}
\arguments{
\item{formula}{A chemical formula.}
\item{dataSources}{Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspideR::get_data_sources()}}
\item{orderBy}{A character string indicating by which parameter the results should be ordered; see Details.}
\item{orderDirection}{A character string indicating in which direction the results should be ordered; see Details.}
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Possible values for \code{orderBy} are:

\code{"recordId"} (default)

\code{"massDefect"}

\code{"molecularWeight"}

\code{"referenceCount"}

\code{"dataSourceCount"}

\code{"pubmedCount"}

\code{"rscCount"}

Possible values for \code{orderDirection} are:

\code{"ascending"} (default)

\code{"descending"}

Says ChemSpider:

"If dataSources is not specified, all known sources are searched. This will take longer."

If successful, returns the \code{queryId} as character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST the formula of Aspirin to get a queryId.

# formula <- "C9H8O4"
# apikey <- "a valid 32-character ChemSpider apikey"
# post_formula(formula = formula, apikey = apikey)
}
