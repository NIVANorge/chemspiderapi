
<!-- README.md is generated from README.Rmd. Please edit that file -->
chemspideR
==========

> R functionalities for ChemSpider's new API services

ChemSpider has introduced a new API syntax in late 2018. `chemspideR` provides R wrappers around the new API services.

The aim of this package is to:

1.  Translate the new ChemSpider API services into R-friendly functions.
2.  Include thorough quality checking *before* the query is send, to avoid using up the query quota on, e.g., spelling errors.
3.  Implement the `R` functionality in a way that is suitable for both `base` and `tidyverse` programming.
4.  Provide (non-ChemSpider) convenience functions to complement the functionality.

The `chemspideR` package is platform independent, but requires a valid ChemSpider API key. While the `chemspideR` package does *not* provide any tools to securely store and access the API key, a look into the excellent [`keyring`](https://github.com/r-lib/keyring) package is recommended. Future updates of `chemspideR` will likely incorporate [`memoise`](https://github.com/r-lib/memoise) features to better handle query IDs.

Coverage
--------

As of 2018-10-16, the following functionalities are implemented (100% of functionality with 67% annotation):

**FILTERING**

<table>
<colgroup>
<col width="37%" />
<col width="38%" />
<col width="24%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">ChemSpider Compound API</th>
<th align="left"><code>chemspideR</code> wrapper</th>
<th align="left"><code>chemspideR</code> help file</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">filter-element-post</td>
<td align="left"><code>chemspideR::post_element()</code></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">filter-formula-batch-post</td>
<td align="left"><code>chemspideR::post_formula_batch()</code></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">filter-formula-batch-queryId-results-get</td>
<td align="left"><code>chemspideR::get_formula_batch_results()</code></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">filter-formula-batch-queryId-status-get</td>
<td align="left"><code>chemspideR::get_formula_batch_status()</code></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">filter-formula-post</td>
<td align="left"><code>chemspideR::post_formula()</code></td>
<td align="left">yes</td>
</tr>
<tr class="even">
<td align="left">filter-inchi-post</td>
<td align="left"><code>chemspideR::post_inchi()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-inchikey-post</td>
<td align="left"><code>chemspideR::post_inchikey()</code></td>
<td align="left">yes</td>
</tr>
<tr class="even">
<td align="left">filter-intrinsicproperty-post</td>
<td align="left"><code>chemspideR::post_intrinsic_property()</code></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">filter-mass-batch-post</td>
<td align="left"><code>chemspideR::post_mass_batch()</code></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">filter-mass-batch-queryId-results-get</td>
<td align="left"><code>chemspideR::get_mass_batch_results()</code></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">filter-mass-batch-queryId-status-get</td>
<td align="left"><code>chemspideR::get_mass_batch_status()</code></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">filter-mass-post</td>
<td align="left"><code>chemspideR::post_mass()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-name-post</td>
<td align="left"><code>chemspideR::post_name()</code></td>
<td align="left">yes</td>
</tr>
<tr class="even">
<td align="left">filter-queryId-results-get</td>
<td align="left"><code>chemspideR::get_results()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-queryId-results-sdf-get</td>
<td align="left"><code>chemspideR::get_sdf()</code> <span style="color:red"><strong>*</strong></span></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">filter-queryId-status-get</td>
<td align="left"><code>chemspideR::get_status()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">filter-smiles-post</td>
<td align="left"><code>chemspideR::post_smiles()</code></td>
<td align="left">yes</td>
</tr>
</tbody>
</table>

**LOOKUPS**

| ChemSpider Compound API | `chemspideR` wrapper             | `chemspideR` help file |
|:------------------------|:---------------------------------|:-----------------------|
| lookups-datasources-get | `chemspideR::get_data_sources()` | yes                    |

**RECORDS**

<table>
<colgroup>
<col width="37%" />
<col width="38%" />
<col width="24%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">ChemSpider Compound API</th>
<th align="left"><code>chemspideR</code> wrapper</th>
<th align="left"><code>chemspideR</code> help file</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">records-batch-post</td>
<td align="left"><code>chemspideR::post_batch()</code></td>
<td align="left">yes</td>
</tr>
<tr class="even">
<td align="left">records-recordId-details-get</td>
<td align="left"><code>chemspideR::get_details()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">records-recordId-externalreferences-get</td>
<td align="left"><code>chemspideR::get_external_references()</code></td>
<td align="left">yes</td>
</tr>
<tr class="even">
<td align="left">records-recordId-image-get</td>
<td align="left"><code>chemspideR::get_image()</code></td>
<td align="left">yes</td>
</tr>
<tr class="odd">
<td align="left">records-recordId-mol-get</td>
<td align="left"><code>chemspideR::get_mol()</code></td>
<td align="left">yes</td>
</tr>
</tbody>
</table>

**TOOLS**

| ChemSpider Compound API      | `chemspideR` wrapper                   | `chemspideR` help file |
|:-----------------------------|:---------------------------------------|:-----------------------|
| tools-convert-post           | `chemspideR::post_convert()`           | yes                    |
| tools-validate-inchikey-post | `chemspideR::post_validate_inchikey()` | yes                    |

**WRITING (`chemspideR` EXCLUSIVE)**

| ChemSpider Compound API | `chemspideR` wrapper        | `chemspideR` help file |
|:------------------------|:----------------------------|:-----------------------|
|                         | `chemspideR::write_image()` | yes                    |
|                         | `chemspideR::write_mol()`   | yes                    |

<span style="color:red">**\***</span> `chemspideR::get_sdf()` downloads the gzipped file, but there is currently no implementation for accessing the (multiple) .mol files contained inside.

Installation
------------

### Dependencies

This packages relies on two essential dependencies, namely `curl` and `jsonlite`. Additional (but not crucial) functionality for handling images is coming from the `png` package.

If not already installed, these packages *should* be installed automatically when installing `chemspideR`. Should this result in trouble, the dependency packages can be installed manually:

``` r
install.packages(c("curl", "jsonlite"))
# install.packages("png")
```

If `curl` and/or `jsonlite` are missing from the R library, all functions of `chemspideR` will throw an error.

### R package

Install the package from GitHub (using `devtools`):

``` r
# install.packages("devtools")
devtools::install_github("NIVANorge/chemspideR")
```

Currently the only tested environment for `chemspideR` is Windows 10, but it *should* install smoothly on macOS and Linux distributions as well. Please open an issue if you run into any troubles.

Usage
-----

This section will be updated with practical examples in the future.

Funding
-------

This package was created at the Norwegian Institute for Water Research (*norsk institutt for vannforskning*; NIVA) in conjunction with NIVA's Computational Toxicology Program (NCTP) at NIVA's Section for Ecotoxicology and Risk Assessment and funded by the Norwegian Research Council (NRC) project 268404: Cumulative Hazard and Risk Assessment of Complex Mixtures and Multiple Stressors (MixRisk).

License
-------

MIT © Raoul Wolf
