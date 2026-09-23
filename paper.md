---
title: 'AISanalyze: streamlining AIS vessel data for environmental analyses in R'
tags:
  - R
  - Automatic Identification System
  - maritime traffic
  - marine ecology
  - vessel tracking
authors:
  - name: Rémi Pigeault
    orcid: 0000-0003-2714-5122
    corresponding: true
    affiliation: 1
  - name: Andreas Ruser
    orcid: 0000-0003-1922-4525
    affiliation: 1
  - name: Nadya C. Ramirez-Martinez
    orcid: 0000-0003-0151-8787
    affiliation: 1
  - name: Ursula Siebert
    orcid: 0000-0002-2556-3948
    equal-contrib: true
    affiliation: "1, 2, 3"
  - name: Anita Gilles
    orcid: 0000-0001-7234-8645
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Institute for Terrestrial and Aquatic Wildlife Research (ITAW), University of Veterinary Medicine Hannover, Foundation, Werftstraße 6, 25761 Büsum, Germany
   index: 1
 - name: Department of Ecoscience, Marine Mammal Research, Aarhus University, Denmark
   index: 2
 - name: Present address -> Institute for Zoo and Wildlife Research, Alfred-Kowalke-Str. 17, 10315 Berlin, Germany
   index: 3
date: 23 July 2026
bibliography: paper.bib
---

# Summary

AISanalyze is an open-source R package that provides an end-to-end workflow for transforming Automatic Identification System (AIS) vessel tracking data into datasets tailored for environmental analyses. AIS is an international vessel tracking system that continuously broadcasts ship identity, position, speed, heading, and other navigational information for maritime safety. Beyond its original purpose, AIS data have become an essential resource for studying maritime traffic in disciplines including marine ecology, conservation, fisheries, transportation, and marine spatial planning [@crum2019; @robards2016; @yang2019].

Despite their widespread use, AIS datasets frequently contain millions of observations together with transmission delays, positional inaccuracies, duplicated records, and incomplete vessel information [@nachtsheim2023; @natale2015; @robards2016]. Preparing these data for scientific analyses is therefore computationally demanding and often requires substantial preprocessing steps before vessel trajectories can be reliably analysed [@kiersztyn2025].

AISanalyze provides an integrated workflow that implements the principal preprocessing steps required for scientific analyses of vessel movements, including travel estimation, correction of GPS errors and transmission delays, identification of non-vessel signals (e.g. base stations and aircraft), trajectory interpolation, extraction of vessel positions around user-defined locations and times, and summarization of vessel characteristics. The package is organized around a small number of functions that preserve the initial data structure and facilitate the use of AIS data in environmental analyses.

AISanalyze is intended for researchers and practitioners working with large AIS datasets, particularly in marine ecology, environmental impact assessment, underwater noise studies, and marine spatial planning. As the use of AIS data continues to expand across scientific and management applications [@yang2019], the package provides a single, computationally efficient workflow to prepare AIS data for downstream analyses and enhance reproducibility. AISanalyze is openly available on GitHub, archived on Zenodo, and distributed under the MIT license.

# Statement of need

Maritime traffic is among the most pervasive human activities in the world oceans and contributes to a wide range of environmental pressures, including underwater noise, collision risk with marine wildlife, habitat disturbance, and greenhouse gas emissions [@dekeling2014; @frisk2012; @halpern2008; @halpern2015]. Quantifying the spatial and temporal distribution of vessel traffic has therefore become an essential component of marine ecological research, environmental impact assessments, and marine spatial planning [@Benhemmalegall2023; @plot2025; @pigeault2024; @paille2024; @pirotta2025; @robards2016].
AIS data provide detailed information on vessel movements over broad spatial and temporal scales and are increasingly combined with environmental observations, species distributions, animal telemetry, and oceanographic data to investigate maritime traffic risks to marine ecosystems, and support evidence-based management decisions [@yang2019]. 

Many of these applications require vessel trajectories to be matched with external datasets (e.g. animal trajectory). However, raw AIS data are not directly suitable for scientific analyses: transmission delays, positional inaccuracies, duplicated records, missing vessel information, and non-vessel signals require substantial preprocessing tasks before vessel trajectories can be reliably analysed [@kiersztyn2025; @robards2016]. Modern AIS datasets frequently contain tens to hundreds of millions of observations [@natale2015], making this computationally demanding. In practice, preparing AIS data may require researchers to combine multiple processing steps and repeatedly implement similar correction, filtering, interpolation, and extraction procedures across projects. A standardized workflow would therefore reduce the time and effort required for these preprocessing tasks, enable a broader range of research applications using AIS data, and support reproducible research.

AISanalyze was developed to this end: it provides a simple, computationally efficient workflow that transforms AIS data into analysis-ready datasets and preserves the initial data structure to facilitate subsequent analyses. The package reduces the technical barriers associated with analyses of large AIS datasets and facilitates reproducible studies on maritime traffic for researchers, environmental practitioners, and policymakers working across disciplines where vessel movements constitute an important explanatory variable.


# State of the field

The growing use of AIS data across scientific disciplines has stimulated the development of software for vessel trajectory processing and maritime traffic analysis [@robards2016]. Many dedicated open-source tools are implemented in Python and provide functionality for AIS data management, cleaning, trajectory reconstruction, interpolation, and analysis. For example, AISdb provides a comprehensive platform for storing, querying, cleaning, and analysing AIS data, including trajectory processing and integration with environmental layers [@spadon2024]. Other tools focus more specifically on trajectory processing, anomaly detection, reconstruction, or movement modelling [@magnussen2023; @park2026; @takahashi2024]. Generic trajectory-processing libraries can also provide complementary functionality for vessel and other movement data [@li2023; @haidri2022].

In R, dedicated tools for AIS preprocessing are more limited. DEPONS2R provides functionality to interpolate AIS tracks at 30-minute intervals [@nabenielsen2026], while gfwr provides access to processed AIS-derived products from Global Fishing Watch [@sancheztapia2026]. These tools address specific applications or data sources but do not provide a comprehensive preprocessing workflow for users working with their own AIS datasets.

AISanalyze addresses this complementary need by focusing on the preprocessing required to obtain reliable and analysis-ready vessel trajectories from AIS data. The package combines correction of positional and temporal errors, identification of non-vessel signals, trajectory interpolation, extraction of vessel positions around user-defined locations and times, and retrieval of vessel characteristics within a single R workflow. This allows researchers to process their own AIS datasets directly within R before combining vessel trajectories with ecological, biological, or environmental observations. AISanalyze therefore complements existing AIS data-processing tools by providing a focused and reproducible workflow for preparing AIS data for downstream environmental analyses.

# Software design

The package is organized around a small number of functions that preserve the initial data structure and facilitate the use of outputs in subsequent R analyses. Each function addresses a specific stage of the preprocessing workflow, allowing users to apply the complete sequence or only the operations required for a particular dataset. This modular structure keeps the user-facing interface simple while allowing individual processing steps to be combined within existing R workflows.

AISanalyze was designed to handle large AIS datasets efficiently. Computationally intensive operations use parallel processing where appropriate, while vectorised algorithms operate directly on tabular data to minimise unnecessary data transformation and repeated manipulation of spatial objects. These design choices reduce execution time and memory requirements when processing large numbers of AIS observations.

A second design objective was to preserve compatibility with the broader R ecosystem and established spatial analysis workflows. All functions return standard data frames that retain the original data structure while appending AIS-derived variables, facilitating downstream analyses without requiring users to modify existing workflows.

The preprocessing methods are based on explicit and reproducible rules. For example, trajectory interpolation uses linear interpolation between observed positions, following approaches adopted in existing AIS processing frameworks [@nabenielsen2026; @park2026; @spadon2024]. Correction procedures identify implausible observations according to defined vessel-specific movement constraints (e.g. travelled speed and distance). The package therefore avoids introducing complex predictive models and remains computationally efficient for large AIS datasets, making the resulting workflow transparent and reproducible.

# Research impact statement 

AISanalyze has already supported peer-reviewed research [@pigeault2024; @wynn2025; @maurer2026] and collaborative projects [@habitatwal; @saturnh2020; @sustainmare] investigating interactions between maritime traffic and marine ecosystems. The package has been used to investigate the short-term relationship between maritime traffic and harbour porpoise distribution in the North Sea between 2015 and 2022, by preparing and combining large AIS datasets to aerial survey data [@pigeault2024]. It has also supported analyses of harbour seal telemetry in the English Channel, where AIS-derived vessel movements were used to investigate the spatial overlap between seals and maritime traffic and to support habitat-selection analyses [@wynn2025]. More recently, AISanalyze was used to investigate the response of harbour seals to their local environment, including vessel presence and activity, in the German Wadden Sea [@maurer2026]. 

AISanalyze is currently used in collaborative research initiatives, including @habitatwal and @sustainmare, to investigate the effects of maritime activities on underwater radiated noise and marine biodiversity, and to support evidence-based conservation in marine spatial planning. The package has also supported research conducted within the @saturnh2020 project to estimate underwater radiated noise from shipping and its impacts on aquatic species. These applications illustrate how AISanalyze can support the integration of vessel traffic data with biological observations, animal movement data, and environmental information across different spatial and temporal scales.

By providing a standardized and reproducible preprocessing workflow, AISanalyze reduces the technical effort required to prepare AIS data for such applications. The package is intended to support transparent and computationally efficient analyses in which maritime traffic can be incorporated as an explanatory variable alongside ecological and environmental datasets. Its integration within the R ecosystem further facilitates the use of AIS data in existing statistical, spatial, and ecological workflows, supporting applications ranging from marine ecological research to environmental assessment and conservation planning.

# AI usage disclosure

Generative artificial intelligence (AI) tools were used to improve the grammar of the manuscript.

# Acknowledgements

This work was supported by the projects CoastalFutures [grant number 03F0911H], funded by the German Federal Ministry of Education and Research (BMBF), and HABITATWal [grant number 3522520200], funded by the German Federal Agency for Nature Conservation (BfN). We acknowledge contributions from Dr. Daniel Alexander Schwarzkopf, Dr. Volker Matthias, Dr. Tobias Schaffeld, and Dr. Dominik Nachtsheim. We would like to thank the German Federal Maritime and Hydrographic Agency (BSH) for providing the AIS data that enabled us to build, check and use this package. The responsibility for the content of this publication lies with the authors.

# References
