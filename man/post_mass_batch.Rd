\name{post_mass_batch}
\alias{post_mass_batch}
\title{POST a batch of up to 20 monoisotopic masses and their ranges to obtain a query ID}
\description{
Functionality to POST up to 20 monoisotopic masses and their range to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_mass_batch(mass, range, dataSources = NULL, orderBy = "recordId",
                orderDirection = "ascending", apikey)
}
\arguments{
\item{mass}{A vector of (double) numbers corresponding to the atomic (monoisotopic) masses you are inquiring. Has to be within the range of [1,11000].}
\item{range}{a vector of (double) numbers corresponding to the rangees for the above masses. Has to be within the range of [0.0001,0.001].}
\item{dataSources}{A character vector specifying which data source to use. Use \code{chemspideR::get_data_sources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.}
\item{orderBy}{A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.}
\item{orderDirection}{A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Says Chemspider:

"If \code{dataSources} is not specified, all known sources are searched. This will take longer.

Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.

Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.

Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."

If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
}
\examples{
## Do not run
## POST two atomic (monoisotopic) masses and their ranges.

# apikey <- "a valid 32-character ChemSpider apikey"
#
# post_mass_batch(mass = c(180, 246), range = c(0.0005, 0.001), apikey = apikey)
}
