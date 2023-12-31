---
title: 'AISanalyze: an R-package to correct, interpolate and extract AIS data'
tags:
  - R
  - AIS data
  - vessel tracks
  - interpolation
  - human activity
  - correction
authors:
  - name: Rémi Pigeault
    orcid: 0000-0003-2714-5122
    affiliation: 1
  - name: Nadya C. Ramirez-Martinez
    orcid: 0000-0003-0151-8787
    affiliation: 1
  - name: Anita Gilles
    orcid: 0000-0001-7234-8645
    affiliation: 1
affiliations:
 - name: Institute for Terrestrial and Aquatic Wildlife Research, University of Veterinary Medicine Hannover, Foundation, Buesum, Germany
   index: 1
date: 08 January 2024
bibliography: paper.bib
---

# Summary

AISanalyze is an R-package developed to specifically correct, in the AIS data, the GPS errors and GPS delays, interpolate (linearly) the vessel positions at the desired times, and extract the locations and information of vessels in a radius around the desired locations/times. The advantage of this package is that the computation time is considerably reduced when performing all these operations, enabling results to be obtained in a few minutes to a few hours depending on the size of the AIS dataset and on the number of locations and times where AIS data are corrected, interpolated and extracted. Furthermore, this package identifies the base-stations and aircrafts from vessels, based on the distance and speed travelled, and allows to extract the information on the length and type of vessels from these AIS data (correcting the errors of length and type present in the AIS). The other advantage is its simplicity: we constructed an all-in-one function, which allows to:
- calculate the distance, time and speed travelled by each vessel;
- correct the GPS errors and GPS delays;
- interpolate the vessel positions at the desired times;
- extract the positions and information of vessels around the desired locations, at the desired times.

A second function allows you to interpolate (linearly) all the AIS data at the desired temporal resolution, regardless of the locations. Finally, a last function extracts the length and type of boat per vessel, removing the errors in the AIS data. This information can later be joined to your interpolated/extracted AIS data.

# Statement of need

Human activities and impacts in the oceans continue to increase (Halpern et al., 2008, 2015), and with them the maritime traffic. AIS (Automatic Identification System) and VMS data (Vessel monitoring system) are the only sources of data on the positions of vessels in the ocean to date, in the absence of collected vessel observations. Studies on maritime traffic's impacts are therefore heavily dependent on these data: however, numerous errors are found, due to GPS errors, GPS delays, errors of receptions and others. This leads to vessel tracks with erroneous positions, speeds, and information (e.g. wrong vessel length and type). Furthermore, the huge size of the AIS data makes any overview complex and very time-consuming. These two points highly limit the possibilities of research on the vessel tracks and densities, related either to the state of the ecosystems, to the abundance of the populations, or the avoidance of the animals to the vessels. Easy-to-use and fast calculation algorithms are required to fill these gaps and allow the community to use AIS data to their full potential and overcome their shortcomings. This R-package opens up a wide range of research possibilities, from studying the effects and distribution of maritime traffic to improving the AIS data treatment and shortcomings.

# Acknowledgements

We acknowledge contributions from Andreas Ruser, Dominik Nachtsheim and Louise Moysan. RP, NCRM and AG were partially supported by project CoastalFutures, funded by the German Federal Ministry of Education and Research (BMBF) under grant number 03F0911H. The responsibility for the content of this publication lies with the authors.

# References

Halpern, B. S., Frazier, M., Potapenko, J., Casey, K. S., Koenig, K., Longo, C., Lowndes, J. S., Rockwood, R. C., Selig, E. R., Selkoe, K. A., & Walbridge, S. (2015). Spatial and temporal changes in cumulative human impacts on the world’s ocean. Nature Communications, 6, 7615. https://doi.org/10.1038/ncomms8615.

Halpern, B. S., Walbridge, S., Selkoe, K. a, Kappel, C. V, Micheli, F., D’Agrosa, C., Bruno, J. F., Casey, K. S., Ebert, C., Fox, H. E., Fujita, R., Heinemann, D., Lenihan, H. S., Madin, E. M. P., Perry, M. T., Selig, E. R., Spalding, M., Steneck, R., & Watson, R. (2008). A Global Map of Human Impact on Marine Ecosystems. Science, 319(5865), 948–952. https://doi.org/10.1126/science.1149345
