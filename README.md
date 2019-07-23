chemspiderapi
================

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/RaoulWolf/chemspiderapi?branch=master&svg=true)](https://ci.appveyor.com/project/RaoulWolf/chemspiderapi)
[![Travis build
status](https://travis-ci.org/RaoulWolf/chemspiderapi.svg?branch=master)](https://travis-ci.org/RaoulWolf/chemspiderapi)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

> R functionalities for ChemSpider’s new API services

ChemSpider has introduced a new API syntax in late 2018, and [the old
ChemSpider API syntax will be shut down at the end of
November 2018](http://link.rsc.org/rsps/m/xSq8Cm8ovjN8-Elm0eYB3Sey61zutqNIUUaMcyc14sQ).
`chemspiderapi` provides R wrappers around the new API services from
ChemSpider.

The aim of this package is to:

1)  Translate the new ChemSpider API services into R-friendly functions.
2)  Include thorough quality checking *before* the query is send, to
    avoid using up the query quota on, e.g., spelling errors.
3)  Implement the R functionality in a way that is suitable for both
    base and tidyverse programming.

`chemspiderapi` relies on API keys to access ChemSpider’s API services.
For this, we recommend storing the ChemSpider API key using
[`keyring`](https://github.com/r-lib/keyring).

To limit the rate of API queries, we recommend using
[`ratelimitr`](https://github.com/tarakc02/ratelimitr) or
`purrr::slowly()` within `tidyverse`.

To handle PNG images (as returned by
`chemspiderapi::get_recordId_image()`)
[`magick`](https://github.com/ropensci/magick) is recommended.

We furthermore recommend [`memoise`](https://github.com/r-lib/memoise)
to “remember” the results of API queries (i.e., to not ruin the API
allowance).

## Installation

### R package

Install the package from GitHub (using the
[`remotes`](https://github.com/r-lib/remotes) package; automatically
installed alongside [devtools](https://github.com/r-lib/devtools)):

``` r
# install.packages("devtools")
remotes::install_github("NIVANorge/chemspiderapi")
```

The development version can be installed with:

``` r
# install.packages("devtools")
remotes::install_github("NIVANorge/chemspiderapi", ref = "dev")
```

Currently the only tested environment for `chemspiderapi` is Windows 10
and macOS, but it *should* install smoothly on Linux machines as well.
Please open an issue if you run into any troubles.

### Dependencies

`chemspiderapi` relies on two essential dependencies, i.e.,
[`curl`](https://github.com/jeroen/curl) and
[`jsonlite`](https://github.com/jeroen/jsonlite).

If not already installed, these packages *should* be installed
automatically when installing `chemspiderapi`. Should this result in
trouble, the dependency packages can be installed manually:

``` r
install.packages(c("curl", "jsonlite"))
```

If `curl` or `jsonlite` are missing from the R library, all functions of
`chemspiderapi` will fail and throw an error.

## Coverage

As of 2019-07-23, the following functionalities are implemented (100%
functionality with 100%
annotation):

**FILTERING**

| ChemSpider Compound API                  | `chemspiderapi` Wrapper               | `chemspiderapi` Help File |
| :--------------------------------------- | :------------------------------------ | :-----------------------: |
| filter-element-post                      | `post_element()`                      |            yes            |
| filter-formula-batch-post                | `post_formula_batch()`                |            yes            |
| filter-formula-batch-queryId-results-get | `get_formula_batch_queryId_results()` |            yes            |
| filter-formula-batch-queryId-status-get  | `get_formula_batch_queryId_status()`  |            yes            |
| filter-formula-post                      | `post_formula()`                      |            yes            |
| filter-inchi-post                        | `post_inchi()`                        |            yes            |
| filter-inchikey-post                     | `post_inchikey()`                     |            yes            |
| filter-intrinsicproperty-post            | `post_intrinsicproperty()`            |            yes            |
| filter-mass-batch-post                   | `post_mass_batch()`                   |            yes            |
| filter-mass-batch-queryId-results-get    | `get_mass_batch_queryId_results()`    |            yes            |
| filter-mass-batch-queryId-status-get     | `get_mass_batch_queryId_status()`     |            yes            |
| filter-mass-post                         | `post_mass()`                         |            yes            |
| filter-name-post                         | `post_name()`                         |            yes            |
| filter-queryId-results-get               | `get_queryId_results()`               |            yes            |
| filter-queryId-results-sdf-get           | `get_queryId_results_sdf()` \*        |            yes            |
| filter-queryId-status-get                | `get_queryId_status()`                |            yes            |
| filter-smiles-post                       | `post_smiles()`                       |            yes            |

\* `get_queryId_results_sdf()` downloads the gzipped base64-encoded
character string, but there is currently no implementation for accessing
the (multiple) .mol files contained
inside.

**LOOKUPS**

| ChemSpider Compound API | `chemspiderapi` Wrapper | `chemspiderapi` Help File |
| :---------------------- | :---------------------- | :-----------------------: |
| lookups-datasources-get | `get_datasources()`     |            yes            |

**RECORDS**

| ChemSpider Compound API                 | `chemspiderapi` Wrapper             | `chemspiderapi` Help File |
| :-------------------------------------- | :---------------------------------- | :-----------------------: |
| records-batch-post                      | `post_batch()`                      |            yes            |
| records-recordId-details-get            | `get_recordId_details()`            |            yes            |
| records-recordId-externalreferences-get | `get_recordId_externalreferences()` |            yes            |
| records-recordId-image-get              | `get_recordId_image()`              |            yes            |
| records-recordId-mol-get                | `get_recordId_mol()`                |            yes            |

**TOOLS**

| ChemSpider Compound API      | `chemspiderapi` Wrapper    | `chemspiderapi` Help File |
| :--------------------------- | :------------------------- | :-----------------------: |
| tools-convert-post           | `post_convert()`           |            yes            |
| tools-validate-inchikey-post | `post_validate_inchikey()` |            yes            |

## Best practices for ChemSpider’s Compound APIs

This section will be updated with practical examples in the future.

The basic workflow order for the above **FILTERING** queries is:

1)  POST Query

2)  GET Status

3)  GET Results (after GET Status returns `"Complete"`)

In practice, this means the following possible workflows can be
implemented:

| POST Query                 | GET Status                           | GET Results                           |
| :------------------------- | :----------------------------------- | :------------------------------------ |
| `post_element()`           | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_formula()`           | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_formula_batch()`     | `get_formula_batch_queryId_status()` | `get_formula_batch_queryId_results()` |
| `post_inchi()`             | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_inchikey()`          | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_intrinsicproperty()` | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_mass()`              | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_mass_batch()`        | `get_mass_batch_queryId_status()`    | `get_mass_batch_queryId_results()`    |
| `post_mass()`              | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_name()`              | `get_queryId_status()`               | `get_queryId_results()`               |
| `post_smiles()`            | `get_queryId_status()`               | `get_queryId_results()`               |

Typically, the result will be one or multiple ChemSpider IDs
(`recordId`). They can be used as input into the above **RECORDS**
queries.

## Funding

This package was created at the [Norwegian Institute for Water Research
(NIVA)](https://www.niva.no/en) in conjunction with [NIVA’s
Computational Toxicology Program
(NCTP)](https://www.niva.no/en/projectweb/nctp) at NIVA’s [Section for
Ecotoxicology and Risk
Assessment](https://www.niva.no/en/research/ecotoxicology_and_risk_assessment)
and funded by [The Research Council of Norway
(RCN)](https://www.forskningsradet.no/en/Home_page/1177315753906),
project 268294: [Cumulative Hazard and Risk Assessment of Complex
Mixtures and Multiple Stressors
(MixRisk)](https://www.forskningsradet.no/prosjektbanken/#/project/NFR/268294/Sprak=en).

## License

MIT Â© Raoul Wolf
