\name{post_convert}
\alias{post_convert}
\title{Convert identifiers}
\description{
Functionality to convert between different chemical identifier formats: InChI, InChIKey, Mol, and SMILES.
}
\usage{
post_convert(input, inputFormat, outputFormat, apikey)
}
\arguments{
\item{input}{A valid .mol file, e.g., as downloaded via \code{chemspider::get_mol()}.}
\item{inputFormat}{A character string indicating which format the input has. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.}
\item{outputFormat}{A character string indicating which type of output is desired. Can be one of the following: \code{InChI}, \code{InChIKey}, \code{Mol}, or \code{SMILES}. See Details for possible conversions.}
\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\details{
Submit an identifier (SMILES, InChI, InChIKey or Mol) as a string and return an identifier in another format (SMILES, InChI, InChIKey or Mol).

Allowed conversions:

from \code{InChI} to \code{InChIKey}

from \code{InChI} to \code{Mol} file

from \code{InChI} to \code{SMILES}

from \code{InChIKey} to \code{InChI}

from \code{InChIKey} to \code{Mol} file

from \code{Mol} file to \code{InChI}

from \code{Mol} file to \code{InChIKey}

from \code{SMILES} to \code{InChI}

Note: The identifier names are NOT case sensitive!

If successful, performs the desired conversion and stores the result as a named ("output") character string.

If not successful, it returns \code{NA}.

This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}
}
\examples{
## Do not run
## convert the InChI string of Aspirin to SMILES.

# apikey <- "a_valid_ChemSpider_API_key"
# post_convert(input = "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)",
#              inputFormat = "InChI",
#              outputFormat = "SMILES",
#              apikey = apikey)
}
