% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FILTERING-post_formula.R
\name{post_formula}
\alias{post_formula}
\title{POST a chemical formula to obtain a query ID}
\usage{
post_formula(
  formula,
  dataSources = NULL,
  orderBy = "recordId",
  orderDirection = "ascending",
  apikey,
  coerce = FALSE,
  simplify = FALSE
)
}
\arguments{
\item{formula}{A character string of a chemical formula.}

\item{dataSources}{Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.}

\item{orderBy}{A character string indicating by which parameter the results should be ordered; see Details.}

\item{orderDirection}{A character string indicating in which direction the results should be ordered; see Details.}

\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}

\item{coerce}{\code{logical}: should the list be coerced to a data.frame? Defaults to \code{FALSE}.}

\item{simplify}{\code{logical}: should the results be simplified to a vector? Defaults to \code{FALSE}.}
}
\value{
Returns the queryId string as (named) character vector.
}
\description{
Functionality to POST a formula to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
}
\details{
Possible values for \code{orderBy} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
\cr
Possible values for \code{orderDirection} are: \code{"ascending"} (default) and \code{"descending"}.\cr
\cr
Says ChemSpider:\cr
\cr
\emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
\cr
If successful, returns the \code{queryId} as character string.
}
\examples{
\dontrun{
## POST the formula of caffeine to get a query ID
formula <- "C8H10N4O2"
apikey <- "a valid 32-character ChemSpider apikey"
post_formula(formula = formula, apikey = apikey)
}
}
\seealso{
\url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula}
}
\author{
Raoul Wolf (\url{https://github.com/RaoulWolf/})
}
