
<!-- README.md is generated from README.Rmd. Please edit that file -->
chemspider
==========

> R functionalities for ChemSpider's new API services

ChemSpider has introduced a new API syntax in late 2018. This package provides R wrappers around the new API services. E.g., `records-recordId-details-get` becomes `chemspider::get_details()`. The `chemspider` package is platform independent, but requires a valid ChemSpider API key. While the `chemspider` package does *not* provide any tools to securely store and access the API key, a look into the excellent `keyring` package is recommended.

`chemspider` is fully `tidyverse` compatible, i.e., it can be used in pipes and in functional programming approaches through `purrr::map()`

Coverage
--------

As of 2018-10-09, the following functionalities are implemented (56% coverage):

**FILTERING**

-   filter-element-post
-   filter-formula-batch-post
-   filter-formula-batch-queryId-results-get
-   filter-formula-batch-queryId-status-get
-   filter-formula-post: `chemspider::post_formula()`
-   filter-inchi-post: `chemspider::post_inchi()`
-   filter-inchikey-post: `chemspider::post_inchikey()`
-   filter-intrinsicproperty-post
-   filter-mass-batch-post
-   filter-mass-batch-queryId-results-get
-   filter-mass-batch-queryId-status-get
-   filter-mass-post: `chemspider::post_mass()`
-   filter-name-post: `chemspider::post_name()`
-   filter-queryId-results-get: `chemspider::get_results()`
-   filter-queryId-results-sdf-get **TROUBLE**
-   filter-queryId-status-get `chemspider::get_status()`
-   filter-smiles-post: `chemspider::post_smiles()`

**LOOKUPS**

-   lookups-datasources-get `chemspider::get_data_sources()`

**RECORDS**

-   records-batch-post **TROUBLE**
-   records-recordId-details-get: `chemspider::get_details()`
-   records-recordId-externalreferences-get: `chemspider::get_external_references()`
-   records-recordId-image-get: `chemspider::get_image()`
-   records-recordId-mol-get: `chemspider::get_mol()`

**TOOLS**

-   tools-convert-post: `chemspider::post_convert()`
-   tools-validate-inchikey-post: `chemspider::post_validate_inchikey()`

**WRITING (`chemspider` EXCLUSIVE)**

-   `chemspider::write_image()`
-   `chemspider::write_mol()`

Installation
------------

### Dependencies

This packages relies on two essential dependencies, namely `httr` and `jsonlite`. Additional (but not crucial) functionality for handling images is coming from the `magick` package; as `magick` comes with several prerequisites, it is recommendable to check with the official documentation first.

The two packages (three with `magick`) can be installed as follows:

``` r
install.packages(c("httr", "jsonlite"))
# install.packages("magick")
```

If `httr` and/or `jsonlite` are missing from the R library, all functions of `chemspider` will throw an error.

### R package

Install the package from GitHub (using `devtools`):

``` r
# install.packages("devtools")
devtools::install_github("NIVANorge/chemspider")
```

Usage
-----

Funding
-------

This package was created as part of NIVA's Computational Toxicity Program (NCTP) and funded by the MixRisk project.

License
-------

MIT © Raoul Wolf
