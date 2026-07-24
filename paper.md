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
    affiliation: "1, 2"
    present_address: Institute for Zoo and Wildlife Research, Alfred-Kowalke-Str. 17, 10315 Berlin, Germany
  - name: Anita Gilles
    orcid: 0000-0001-7234-8645
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Institute for Terrestrial and Aquatic Wildlife Research, University of Veterinary Medicine Hannover, Foundation, Buesum, Germany
   index: 1
 - name: Department of Ecoscience, Marine Mammal Research, Aarhus University, Denmark
   index: 2
date: 23 July 2026
bibliography: paper.bib
---

# Summary

AISanalyze is an open-source R package that provides an end-to-end workflow for transforming Automatic Identification System (AIS) vessel tracking data into analysis-ready datasets. AIS is an international radio-based vessel tracking system that continuously broadcasts ship identity, position, speed, heading, and other navigational information for maritime safety. Beyond its original purpose, AIS data have become an essential resource for studying maritime traffic in disciplines including marine ecology, conservation, fisheries, transportation, and marine spatial planning [@crum2019; @robards2016; @yang2019].
Despite their widespread use, AIS datasets frequently contain millions of observations together with transmission delays, positional inaccuracies, duplicated records, and incomplete vessel information [@nachtsheim2023; @natale2015; @robards2016]. Preparing these data for scientific analyses is therefore computationally demanding and often requires substantial preprocessing before vessel trajectories can be reliably analysed [@kiersztyn2025].
AISanalyze provides an integrated workflow that implements the principal preprocessing operations required for scientific analyses of vessel movements, including travel estimation, correction of GPS errors and transmission delays, identification of non-vessel signals (e.g. base stations and aircraft), trajectory interpolation, extraction of vessel positions around user-defined locations and times, and summarization of vessel characteristics. The package is organized around a small number of user-facing functions that preserve a consistent data structure throughout the workflow, facilitating integration into existing R analysis pipelines.
AISanalyze is intended for researchers and practitioners working with large AIS datasets, particularly in marine ecology, environmental impact assessment, underwater noise studies, and marine spatial planning. As the use of AIS data continues to expand across scientific and management applications [@yang2019], the package provides a reproducible and computationally efficient framework for preparing AIS data for downstream statistical and spatial analyses. AISanalyze is openly available on GitHub, archived on Zenodo, and distributed under the MIT license.

# Statement of need

Maritime traffic is among the most pervasive human activities in the world's oceans and contributes to a wide range of environmental pressures, including underwater noise, collision risk with marine wildlife, habitat disturbance, and greenhouse gas emissions [@dekeling2014; @frisk2012; @halpern2008; @halpern2015]. Quantifying the spatial and temporal distribution of vessel traffic has therefore become an essential component of marine ecological research, environmental impact assessments, and marine spatial planning [@Benhemmalegall2023; @plot2025; @pigeault2024; @pirotta2025; @robards2016].
AIS data provide detailed information on vessel movements over broad spatial and temporal scales and are increasingly combined with environmental observations, species distributions, animal telemetry, and oceanographic data to investigate the effects of maritime traffic on marine ecosystems and to support evidence-based management decisions [@yang2019]. Many of these applications require vessel trajectories to be matched with external datasets, creating preprocessing requirements that extend beyond trajectory reconstruction alone.
Despite their widespread use, raw AIS data are not directly suitable for scientific analyses: transmission delays, positional inaccuracies, duplicated records, missing vessel information, and non-vessel signals require substantial preprocessing before vessel trajectories can be reliably analysed [@kiersztyn2025; @robards2016]. Modern AIS datasets frequently contain tens to hundreds of millions of observations [@natale2015], making routine preprocessing computationally demanding and often requiring researchers to develop project-specific workflows that are difficult to reproduce and maintain.
To date, these preprocessing tasks were commonly implemented through project-specific scripts that were repeatedly redeveloped across independent studies, e.g. @ Benhemmalegall2023, @nachtsheim2023, @paille2024. Such workflows reduce reproducibility, increase development time, and create unnecessary barriers for researchers wishing to integrate maritime traffic into ecological or environmental analyses. A standardized preprocessing framework therefore benefits both software reuse and the reproducibility of scientific studies relying on AIS data.
AISanalyze was developed to simplify this preprocessing stage by providing an integrated workflow that transforms raw AIS data into analysis-ready datasets while preserving compatibility with downstream statistical and spatial analyses. By reducing the technical barriers associated with large AIS datasets, the package facilitates reproducible studies of maritime traffic for researchers, environmental practitioners, and policymakers working across disciplines where vessel movements constitute an important explanatory variable.

# State of the field

The increasing use of AIS data across scientific disciplines has stimulated the development of numerous software tools dedicated to vessel trajectory processing, movement reconstruction, and maritime traffic analysis [@robards2016]. Historically, AIS preprocessing was largely performed through project-specific scripts, limiting reproducibility and requiring researchers to repeatedly implement similar correction and interpolation procedures. More recently, several open-source tools have been developed to facilitate AIS analyses by addressing specific aspects of vessel trajectory processing, including database management, anomaly detection, trajectory reconstruction, and movement modelling [@magnussen2023; @nabenielsen2026; @park2026; @spadon2024; @takahashi2024].
These tools provide valuable capabilities for their intended applications but often require users to combine multiple software packages or develop additional scripts to complete a typical scientific workflow [@nabenielsen2026; @spadon2024]. Furthermore, some reconstruction methods rely on historical vessel behaviour or predictive models to estimate vessel positions [@magnussen2023; @park2026; @takahashi2024]. While these approaches are appropriate for many applications, they introduce modelling assumptions that may not be desirable when the objective is to preserve the original vessel observations while preparing datasets for subsequent ecological or environmental analyses.
AISanalyze was developed to address a complementary need. Rather than focusing on the development of a new trajectory reconstruction algorithm, the package provides a reproducible preprocessing workflow that prepares AIS datasets for downstream scientific analyses. It integrates the principal preprocessing operations commonly required by marine ecologists and environmental scientists within a single software package, including travel estimation, GPS correction, identification of non-vessel signals, trajectory interpolation, extraction of vessel positions around user-defined locations and times, and retrieval of vessel characteristics.
A key design choice of AISanalyze is the use of deterministic preprocessing algorithms that can be readily interpreted by users. For example, vessel trajectories are interpolated using linear interpolation between consecutive AIS positions, following approaches adopted in several existing AIS processing frameworks [@nabenielsen2026; @park2026; @spadon2024]. This choice minimizes methodological assumptions, preserves transparency, and remains computationally efficient for large AIS datasets. AISanalyze therefore complements existing AIS software by providing an integrated workflow that reduces the need for project-specific preprocessing scripts while facilitating reproducible ecological and environmental analyses.

# Software design

AISanalyze was intentionally designed around a small number of user-facing functions representing the principal stages of AIS preprocessing. Rather than exposing numerous specialized functions, the package adopts a modular workflow in which each function performs a well-defined preprocessing task while remaining independent from the others. This design minimizes the learning curve for new users while preserving the flexibility to apply individual processing steps according to the requirements of a given study.
A central design objective was to preserve compatibility with existing R workflows. Consequently, all functions accept and return standard data frames whose structure closely matches the original AIS dataset. Rather than replacing existing information, derived variables such as travel distance, travel time, corrected vessel positions, or interpolated observations are appended to the original data. Preserving the original observations while augmenting them with derived variables allows users to inspect every preprocessing step and facilitates the integration of AISanalyze within downstream statistical, spatial, and ecological analyses.
Transparency and reproducibility were also important design considerations. Wherever possible, AISanalyze implements deterministic algorithms whose behaviour can be directly interpreted by users. For example, vessel trajectories are interpolated using linear interpolation rather than model-based prediction, allowing users to preserve observed vessel movements while avoiding additional modelling assumptions. Similarly, GPS correction procedures rely on explicit movement constraints derived from estimated vessel travel rather than statistical inference models.
AISanalyze is structured into six user-facing functions with additional (internal) helper functions to simplify package maintenance, facilitate unit testing, and enable future extensions without modifying the public interface. Because every processing step returns standard R data frames, AISanalyze integrates naturally with the broader R ecosystem for statistical modelling, spatial analyses, movement ecology, and visualization.

# Research impact statement 

AISanalyze has already supported peer-reviewed studies [@pigeault2024; @wynn2025] and ongoing collaborative research projects [@habitatwal; @saturnh2020; @sustainmare] investigating the ecological impacts of maritime traffic on marine ecosystems. These applications demonstrate the package's ability to efficiently preprocess large AIS datasets and integrate vessel trajectories with biological observations, environmental data, and spatial analyses across broad spatial and temporal scales.
The package has been used to investigate the short-term relationship between maritime traffic and harbour porpoise distribution throughout the North Sea between 2015 and 2022 by combining extensive AIS records with aerial survey data [@pigeault2024]. It has also been employed to quantify the spatial overlap between harbour seals tracked by telemetry and maritime traffic in the English Channel, enabling the integration of vessel trajectories with habitat selection analyses [@wynn2025]. Collectively, these studies illustrate how standardized AIS preprocessing facilitates ecological analyses that combine vessel movements with independent environmental datasets.
Beyond published studies, AISanalyze is currently used within several collaborative research initiatives, including SustainMare, SATURN, and HABITATWal, which investigate the impacts of maritime activities on marine biodiversity, underwater noise, and ecosystem management. The package also supports applications requiring standardized vessel characteristics, such as ship type and vessel dimensions, which can be integrated with existing ship-noise source models and other analyses of maritime environmental pressures.
By providing a transparent, reproducible, and computationally efficient preprocessing workflow, AISanalyze reduces the technical barriers associated with analysing large AIS datasets and facilitates the incorporation of maritime traffic into ecological and environmental research.

# AI usage disclosure

Generative artificial intelligence (AI) tools were used to improve the grammar of the manuscript.

# Acknowledgements

This work was supported by the projects CoastalFutures [grant number 03F0911H], funded by the German Federal Ministry of Education and Research (BMBF), and HABITATWal [grant number 3522520200], funded by the German Federal Agency for Nature Conservation (BfN). We acknowledge contributions from Daniel Alexander Schwarzkopf, Volker Matthias, Tobias Schaffeld, and Dominik Nachtsheim. We would like to thank the German Federal Maritime and Hydrographic Agency (BSH) for providing the AIS data that enabled us to build, check and use this package. The responsibility for the content of this publication lies with the authors.

# References
