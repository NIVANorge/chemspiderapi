---
title: "chemspiderapi: An R package to access ChemSpider's API services"
tags:
  - R
  - ChemSpider
  - application programming interface
  - chemistry
  - functional programming
  - package
authors:
  - name: Raoul Wolf
    orcid: 0000-0001-5971-8525
    affiliation: 1
affiliations:
 - name: Section for Ecotoxicology and Risk Assessment, Norwegian Institute for Water Research (NIVA)
   index: 1
date: 23 July 2019
bibliography: paper.bib
---

# Summary

Researchers in natural sciences increasingly must access big chunks of chemical information, e.g., in modern read-across procedures [@pawar:2019]. Examples include the identification of chemicals or retrieval of physico-chemical properties.  The Royal Society of Chemistry offers its chemical structure database ChemSpider ([https://www.chemspider.com](https://www.chemspider.com)) as an online tool for fast searches of chemicals, weather it is by chemical name, structural formulae, or chemical identifiers. To increase usability, ChemSpider offers a suite of API services ([https://developer.rsc.org/compounds-v1/apis](https://developer.rsc.org/compounds-v1/apis)) for functional programming and automation. With this comes the need to incorporate these API services into data analysis procedures, e.g., in statistical open-source language R [@rcoreteam:2019]. In late 2018, these API services were completely overhauled, resulting in breaking changes for established workflows. Additionally, use of the API services without a valid API key has been disabled. 

``chemspiderapi`` is a lightweight, minimal-dependency R package to sanely use ChemSpider’s API services. It provides wrappers around all of ChemSpider's API services, runs extensive sanity checking *before* queries, and contains information on best practices for using ChemSpider's API services. The only necessary dependencies are fellow R packages ``curl`` [@ooms:2019] and ``jsonlite`` [@ooms:2014]. Because of this lightweight focus, ``chemspiderapi`` consciously does *not* provide built-in functionalities for:

1) storing and accessing API keys, 

2) rate-limiting API queries, 

3) handling PNG images, or 

4) handling MOL files. 

Instead, separate short vignettes provide examples on how to handle those issues using different coding styles and packages. This encourages users to find solutions that best fit their coding styles and workflows. 

``chemspiderapi`` is fully capable of being deployed in functional programming and comes with an extensive suite of sanity checking functions that verify input values *before* running API queries. This prevents unnecessary queries because the wrong input was used, or the API key was forgotten. 

As such, ``chemspiderapi`` is a powerful addition to etsablished packages like ``rpubchem`` [@guha:2016] and ``webchem`` [@szocs:2015], which provide API functionalities for other online databases.

# Acknowledgements

``chemspiderapi`` was created in conjunction with [NIVA's Computational Toxicology Program (NCTP)](https://www.niva.no/en/projectweb/nctp) and funded by [The Research Council of Norway
(RCN)](https://www.forskningsradet.no/en/Home_page/1177315753906), project 268294: [Cumulative Hazard and Risk Assessment of Complex
Mixtures and Multiple Stressors
(MixRisk)](https://www.forskningsradet.no/prosjektbanken/#/project/NFR/268294/Sprak=en).

# References