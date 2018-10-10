\name{post_mass}
\alias{post_mass}
\title{POST an atomic mass and its range}
\description{
Functionality to POST an atomic mass and its range, in order to obtain a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.
}
\usage{
post_mass(mass, range, dataSources = NULL, orderBy = "recordId",
          orderDirection = "ascending", apikey)
}
\arguments{
\item{mass}{A (double) number corresponding to the atomic mass (Da or g/mol) you are inquiring. Has to be within the range of [1,11000].}
\item{range}{The range for the above mass, also as (double) number. Has to be within the range of [0.0001,100].}
\item{dataSources}{Recommended: A character vector specifying which data source to use. Use \code{chemspideR::get_data_sources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.}
\item{orderBy}{Optional: A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.}
\item{orderDirection}{Optional: A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Says Chemspider:

"Submit mass as a double between 1 and 11000 Atomic Mass Units, and a range between 0.0001 and 100 via POST. For example, if you specify a mass of 40 and a range of 5, all monoisotopic masses between 35 and 45 are searched. [...]

If \code{dataSources} is not specified, all known sources are searched. This will take longer.

Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.

Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.

Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."

If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspideR::get_status()} and \code{chemspideR::get_results()}.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## Post the approximate atomic mass of aspirin and a sensible range.

# apikey <- "a_valid_ChemSpider_API_key"
#
# post_mass(mass = 180, range = 0.2, apikey = apikey)
}
