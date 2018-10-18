\name{post_formula_batch}
\alias{post_formula_batch}
\title{POST a batch of up to 100 formula}
\description{
Functionality to POST a formula to obtain a \code{queryId} for use in \code{chemspider::get_status()} and \code{chemspider::get_results()}.
}
\usage{
post_formula_batch(formulas, dataSources = NULL, orderBy = "recordId",
                   orderDirection = "ascending", apikey)
}
\arguments{
\item{formulas}{A character vector of up to 100 chemical formulas.}
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
## POST the formulas of Aspirin and Cocaine to get a queryId.

# formulas <- c("C9H8O4", "C17H21NO4")
# apikey <- "a valid 32-character ChemSpider apikey"
# post_formula_batch(formulas = formulas, apikey = apikey)
}
