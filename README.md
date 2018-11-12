chemspiderapi
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
> `R` functionalities for ChemSpider's new API services

ChemSpider has introduced a new API syntax in late 2018, and [the old ChemSpider API syntax will be shut down at the end of November 2018](http://link.rsc.org/rsps/m/xSq8Cm8ovjN8-Elm0eYB3Sey61zutqNIUUaMcyc14sQ). `chemspiderapi` provides `R` wrappers around the new API services from ChemSpider.

The aim of this package is to:

1.  Translate the new ChemSpider API services into `R`-friendly functions.
2.  Include thorough quality checking *before* the query is send, to avoid using up the query quota on, e.g., spelling errors.
3.  Implement the `R` functionality in a way that is suitable for both `base` and `tidyverse` programming.
4.  Provide (non-ChemSpider) convenience functions to complement the functionality.

The `chemspiderapi` package is platform independent, but requires a valid ChemSpider API key. While the `chemspiderapi` package does *not* provide any tools to securely store and access the API key, a look into the excellent [`keyring`](https://github.com/r-lib/keyring) package is recommended. To "remember" the results of API queries (i.e., to not ruin the API allowance), [`memoise`](https://github.com/r-lib/memoise) is recommended. Finally, any rate limitations can be created with functionalities from [`ratelimitr`](https://github.com/tarakc02/ratelimitr).

Installation
------------

### R package

Install the package from GitHub (using `devtools`):

``` r
# install.packages("devtools")
remotes::install_github("NIVANorge/chemspiderapi")
```

Currently the only tested environment for `chemspiderapi` is Windows 10, but it *should* install smoothly on macOS and Linux distributions as well. Please open an issue if you run into any troubles.

### Dependencies

`chemspiderapi` relies on two essential dependencies, namely `curl` and `jsonlite`. Additional (but not crucial) functionality for handling images is coming from the `png` package.

If not already installed, these packages *should* be installed automatically when installing `chemspiderapi`. Should this result in trouble, the dependency packages can be installed manually:

``` r
install.packages(c("curl", "jsonlite", "png"))
```

If `curl` or `jsonlite` are missing from the R library, all functions of `chemspiderapi` will fail and throw an error.

Coverage
--------

As of 2018-11-12, the following functionalities are implemented (100% functionality with 100% annotation):

**FILTERING**

<table>
<colgroup>
<col width="33%" />
<col width="43%" />
<col width="22%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">ChemSpider Compound API</th>
<th align="left"><code>chemspiderapi</code> Wrapper</th>
<th align="center"><code>chemspiderapi</code> Help File</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">filter-element-post</td>
<td align="left"><code>chemspiderapi::post_element()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-formula-batch-post</td>
<td align="left"><code>chemspiderapi::post_formula_batch()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-formula-batch-queryId-results-get</td>
<td align="left"><code>chemspiderapi::get_formula_batch_queryId_results()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-formula-batch-queryId-status-get</td>
<td align="left"><code>chemspiderapi::get_formula_batch_queryId_status()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-formula-post</td>
<td align="left"><code>chemspiderapi::post_formula()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-inchi-post</td>
<td align="left"><code>chemspiderapi::post_inchi()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-inchikey-post</td>
<td align="left"><code>chemspiderapi::post_inchikey()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-intrinsicproperty-post</td>
<td align="left"><code>chemspiderapi::post_intrinsicproperty()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-mass-batch-post</td>
<td align="left"><code>chemspiderapi::post_mass_batch()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-mass-batch-queryId-results-get</td>
<td align="left"><code>chemspiderapi::get_mass_batch_queryId_results()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-mass-batch-queryId-status-get</td>
<td align="left"><code>chemspiderapi::get_mass_batch_queryId_status()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-mass-post</td>
<td align="left"><code>chemspiderapi::post_mass()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-name-post</td>
<td align="left"><code>chemspiderapi::post_name()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-queryId-results-get</td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-queryId-results-sdf-get</td>
<td align="left"><code>chemspiderapi::get_queryId_results_sdf()</code> *</td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">filter-queryId-status-get</td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-smiles-post</td>
<td align="left"><code>chemspiderapi::post_smiles()</code></td>
<td align="center">yes</td>
</tr>
</tbody>
</table>

\* `chemspiderapi::get_queryId_results_sdf()` downloads the gzipped base64-encoded character string, but there is currently no implementation for accessing the (multiple) .mol files contained inside.

**LOOKUPS**

| ChemSpider Compound API | `chemspiderapi` Wrapper            | `chemspiderapi` Help File |
|:------------------------|:-----------------------------------|:-------------------------:|
| lookups-datasources-get | `chemspiderapi::get_datasources()` |            yes            |

**RECORDS**

<table style="width:100%;">
<colgroup>
<col width="33%" />
<col width="42%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">ChemSpider Compound API</th>
<th align="left"><code>chemspiderapi</code> Wrapper</th>
<th align="center"><code>chemspiderapi</code> Help File</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">records-batch-post</td>
<td align="left"><code>chemspiderapi::post_batch()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">records-recordId-details-get</td>
<td align="left"><code>chemspiderapi::get_recordId_details()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">records-recordId-externalreferences-get</td>
<td align="left"><code>chemspiderapi::get_recordId_externalreferences()</code></td>
<td align="center">yes</td>
</tr>
<tr class="even">
<td align="left">records-recordId-image-get</td>
<td align="left"><code>chemspiderapi::get_recordId_image()</code></td>
<td align="center">yes</td>
</tr>
<tr class="odd">
<td align="left">records-recordId-mol-get</td>
<td align="left"><code>chemspiderapi::get_recordId_mol()</code></td>
<td align="center">yes</td>
</tr>
</tbody>
</table>

**TOOLS**

| ChemSpider Compound API      | `chemspiderapi` Wrapper                   | `chemspiderapi` Help File |
|:-----------------------------|:------------------------------------------|:-------------------------:|
| tools-convert-post           | `chemspiderapi::post_convert()`           |            yes            |
| tools-validate-inchikey-post | `chemspiderapi::post_validate_inchikey()` |            yes            |

**WRITING (`chemspiderapi` EXCLUSIVE)**

| ChemSpider Compound API | `chemspiderapi` Wrapper        | `chemspiderapi` Help File |
|:------------------------|:-------------------------------|:-------------------------:|
|                         | `chemspiderapi::write_image()` |            yes            |
|                         | `chemspiderapi::write_mol()`   |            yes            |

Best practices for ChemSpider's Compound APIs
---------------------------------------------

This section will be updated with practical examples in the future.

The basic workflow order for the above **FILTERING** queries is:

1.  POST Query

2.  GET Status

3.  GET Results (after GET Status returns `"Complete"`)

In practice, this means the following possivle workflows can be implemented:

<table>
<colgroup>
<col width="29%" />
<col width="35%" />
<col width="35%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">POST Query</th>
<th align="left">GET Status</th>
<th align="left">GET Results</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_element()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="even">
<td align="left"><code>chemspiderapi::post_formula()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_formula_batch()</code></td>
<td align="left"><code>chemspiderapi::get_formula_batch_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_formula_batch_queryId_results()</code></td>
</tr>
<tr class="even">
<td align="left"><code>chemspiderapi::post_inchi()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_inchikey()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="even">
<td align="left"><code>chemspiderapi::post_intrinsicproperty()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_mass()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="even">
<td align="left"><code>chemspiderapi::post_mass_batch()</code></td>
<td align="left"><code>chemspiderapi::get_mass_batch_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_mass_batch_queryId_results()</code></td>
</tr>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_mass()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="even">
<td align="left"><code>chemspiderapi::post_name()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
<tr class="odd">
<td align="left"><code>chemspiderapi::post_smiles()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_status()</code></td>
<td align="left"><code>chemspiderapi::get_queryId_results()</code></td>
</tr>
</tbody>
</table>

Typically, the result will be one or multiple ChemSpider IDs (`recordId`). They can be used as input into the above **RECORDS** queries.

Funding
-------

This package was created at the [Norwegian Institute for Water Research (NIVA)](https://www.niva.no/en) in conjunction with NIVA's Computational Toxicology Program (NCTP) at NIVA's [Section for Ecotoxicology and Risk Assessment](https://www.niva.no/en/research/environmental-toxicology) and funded by [The Research Council of Norway (RCN)](https://www.forskningsradet.no/en/Home_page/1177315753906), project 268294: [Cumulative Hazard and Risk Assessment of Complex Mixtures and Multiple Stressors (MixRisk)](https://www.forskningsradet.no/prosjektbanken/#/project/NFR/268294/Sprak=en).

License
-------

MIT © Raoul Wolf
