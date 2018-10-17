\name{get_datas_sources}
\alias{get_data_sources}
\title{A listing of all (external) ChemSpider data sources}
\description{
A listing of all (external) ChemSpider data sources.
}
\usage{
get_data_sources(apikey)
}
\arguments{
\item{apikey}{A valid 32-character string with a valid key for ChemSpider's API services.}
}
\details{
If succesfull, return a character vector listing all available (external) data sources.

If not succesfull, returns a warning and NA.

This function is most useful for narrowing down \code{dataSources} in other functionalities, e.g., \code{chemspider::get_references()}.
}
\examples{
## Do not run

# apikey <- "a valid 32-character ChemSpider apikey"
# get_data_sources(apikey = apikey)
}
