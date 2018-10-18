\name{post_element}
\alias{post_element}
\title{POST a vector of elements to obtain a query ID}
\description{
Functionality to POST up to 15 elements to include and up to 100 elements to exclude to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_element(includeElements, excludeElements, includeAll = FALSE, complexity = "any",
             isotopic = "any", orderBy = "recordId", orderDirection = "ascending",
             apikey)
}
\arguments{
\item{includeElements}{A character vector of elements to include; maximum length 15.}
\item{excludeElements}{A character vector of elements to exclude; maximum length 100.}
\item{includeAll}{Only look for records containing ALL elements of \code{includeElement}?}
\item{complexity}{See Details.}
\item{isotopic}{See Details.}
\item{orderBy}{A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.}
\item{orderDirection}{A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Says Chemspider:

"Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.

Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.

Valid values for \code{orderDirection} are \code{ascending}, \code{descending}.

The default behavior is to consider records which contain any of the elements listed in the \code{includeElements} [vector]. Setting \code{includeAll} to [\code{TRUE}] will only consider records which contain all the elements listed in the \code{includeElements} [vector].

Valid values for \code{complexity} are \code{"any"}, \code{"single"}, or \code{"multiple"} whereby a compound with a complexity of multiple has more than one disconnected system in it or a metal atom or ion.

Valid values for \code{isotopic} are \code{"any"}, \code{"labeled"}, or \code{"unlabeled"}."

If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST the elements for Aspirin and exclude certain elements.

# includeElements <- c("C", "H", "O")
# excludeElements <- c("K", "Na", "N", "Cl", "F")
# apikey <- "a valid 32-character ChemSpider apikey"
# post_element(includeElements, excludeElements, apikey = apikey)
}
