\name{get_datas_ources}
\alias{get_data_sources}
\title{A listing of all (external) ChemSpider data sources}
\description{
A listing of all (external) ChemSpider data sources.
}
\usage{
get_data_sources(apikey)
}
\arguments{
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
This function returns a data frame listing all available (external) data sources.

This function is most useful for narrowing down \code{dataSources} in \code{chemspider::get_references()}.
}
\examples{
## Do not run

# apikey <- "a_valid_ChemSpider_API_key"
# get_data_sources(apikey = apikey)
}
