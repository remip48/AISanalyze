# Summary

AISanalyze is an R-package developed specifically to correct GPS errors
in AIS data (Automatic Identification System data), linearly interpolate
vessel positions at the desired times, and extract the locations and
information of vessels around desired locations/times. The advantage of
this package is that the computation time is considerably reduced to
perform these operations, enabling results to be obtained in few minutes
to few hours depending on the size of AIS data and the number of points
to extract. The package distinguishes vessels from base-stations and
aircrafts in AIS data (based on the distance and speed travelled),
allowing users to remove the latter if needed. The package also extracts
information on vessel length and type, excluding errors originally
present in the AIS data. First, an all-in-one function was constructed
to analyse, correct, and extract AIS data around desired locations,
therefore:

- calculating the distance, time and speed travelled by each vessel;

- identifying base stations and aircrafts in the AIS;

- correcting GPS errors and GPS delays;

- interpolating the vessel positions at the customized times;

- extracting the vessel positions and their information around the
  desired locations/times.

However, each function is also available individually. Second, a
function allows to linearly interpolate AIS data at the desired temporal
resolution, regardless of the location. Third, a last function extracts
the length and type of vessel per MMSI (Maritime Mobile Service
Identity), removing the errors present in the AIS data. This information
can later be added to the interpolated/extracted AIS data.

# Statement of need

Human activities and their impacts on the ocean and their ecosystem
components continue to increase (Halpern et al., 2008, 2015), including
maritime traffic. AIS (Automatic Identification System) and VMS data
(Vessel Monitoring System) are the only sources of global data on vessel
positions in real time to date. Studies on the impacts of maritime
traffic are therefore heavily dependent on these data: however, numerous
errors are found, due to GPS errors, GPS delays, and errors of
receptions. This leads to vessel tracks with erroneous positions,
speeds, and information (e.g., wrong vessel length and type). The huge
size of AIS data makes any overview complex and highly time-consuming.
These two points greatly limit research possibilities related to vessel
tracks and densities in a wider context. Easy-to-use and fast
calculation algorithms are required to fill these gaps and allow the
community to use AIS data to carry out these researches. AISanalyze
opens up a wide range of research possibilities, including the study of
maritime traffic effects and distribution and the improvement of AIS
data shortcomings and processing (e.g. Pigeault et al., 2024).

# Acknowledgements

We acknowledge contributions from Daniel Alexander Schwarzkopf, Volker
Matthias, Tobias Schaffeld, and Dominik Nachtsheim. We would like to
thank the German Federal Maritime and Hydrographic Agency (BSH) for
providing the AIS data that enabled us to build, check and use this
package. This work was conducted in the framework of the CoastalFutures
project, funded by the German Federal Ministry of Education and Research
(BMBF) under grant number 03F0911H. The responsibility for the content
of this publication lies with the authors.

# References

Halpern, B. S., Frazier, M., Potapenko, J., Casey, K. S., Koenig, K.,
Longo, C., Lowndes, J. S., Rockwood, R. C., Selig, E. R., Selkoe, K. A.,
& Walbridge, S. (2015). Spatial and temporal changes in cumulative human
impacts on the world’s ocean. Nature Communications, 6, 7615.
<https://doi.org/10.1038/ncomms8615>.

Halpern, B. S., Walbridge, S., Selkoe, K. a, Kappel, C. V, Micheli, F.,
D’Agrosa, C., Bruno, J. F., Casey, K. S., Ebert, C., Fox, H. E., Fujita,
R., Heinemann, D., Lenihan, H. S., Madin, E. M. P., Perry, M. T., Selig,
E. R., Spalding, M., Steneck, R., & Watson, R. (2008). A Global Map of
Human Impact on Marine Ecosystems. Science, 319(5865), 948–952.
<https://doi.org/10.1126/science.1149345>

Pigeault, R., Ruser, A., Ramírez-Martínez, N.C., Geelhoed, S.C.V.,
Haelters, J., Nachtsheim, D.A., Schaffeld, T., Sveegaard, S., Siebert,
U., Gilles, A. (2024). Maritime traffic alters distribution of the
harbour porpoise in the North Sea. Mar. Pollut. Bull. 208, 116925.
<https://doi.org/10.1016/j.marpolbul.2024.116925>.
