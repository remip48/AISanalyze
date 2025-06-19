
# LAST UPDATE:

<font size='6'> **19 June 2025** </font> <br> <font size='5'>**Please
update if your installation of AISanalyze is prior to this
revision.**</font>

<br>Last updates: <br>correction in AIScorrect_speed for stationary
boats; correction of time_travelled and distance_travelled in the case
of several consecutive interpolated points.

### Functionality of the package

AISanalyze is an R-package developed to specifically correct GPS errors
in AIS data, interpolate (linearly) the vessel positions at the desired
times, and extract the locations and information of vessels around
desired locations, at desired times. The advantage of this package is
that the computation time is considerably reduced to perform these
operations, enabling results to be obtained in few minutes to few hours
depending on the size of AIS data and the number of points to extract.
Furthermore, this package identifies the vessels from the base stations
and aircrafts in the AIS data, based on the distance and speed
travelled, allowing to filter only the first if desired. It extracts
information on vessel length and type as well, by excluding errors on
vessel length and type originally present in the AIS data. First, an
all-in-one function was constructed to analyze, correct, and extract AIS
data around desired locations, therefore:

-   calculating the distance, time and speed travelled by each
    vessel;<br>
-   identifying base stations and aircrafts in the AIS;<br>
-   correcting GPS errors and GPS delays;<br>
-   interpolating the vessel positions at the customized times;<br>
-   extracting the vessel positions and their information around the
    desired locations, at the desired times.

However, each function is available individually as well. Second, a
further function allows to interpolate (linearly) all AIS data at the
desired temporal resolution, regardless of the location. Finally, a last
function extracts the length and type of vessel per MMSI (Maritime
Mobile Service Identity), removing the errors present in the AIS data.
This information can later be added to the interpolated/extracted AIS
data.

### AISanalyze R-package

#### 1. Quick overview

You can:

-   calculate the distance and time travelled for each vessel (defined
    by MMSI) between every AIS signal, as well as the corresponding
    speed (km/h), using **AIStravel** function;

-   interpolate the MMSI locations at the desired times with the
    function **AISinterpolate_at**. To restrict the computation time and
    not interpolate tracks of vessels that are not of interest, the
    function asks to associate every time with a location (longitude,
    latitude): the AIS data are then kept only within a defined radius
    (in meters, that the user can change in the function parameters)
    around these locations.

    -   This function allows, for example, interpolating the vessel
        locations around your survey / bio-logging data at the time you
        wish. <br>
    -   You can enter, as input data, a data frame with several rows,
        each one containing different combinations of time\~location (in
        order to extract vessel locations along the track of your survey
        / bio-logging data, for example). <br>
    -   Vessel tracks are interpolated based on AIS data that are within
        the radius around all your input locations; this **radius**
        (default value: 200 km) must therefore be large enough to
        account for GPS or AIS gaps, for example in offshore areas. <br>
    -   The GPS errors and unrealistic speeds are also corrected in the
        process of interpolation.<br>
    -   You can filter out the base-stations and aircrafts from the AIS
        data using **filter_station** = TRUE and **filter_high_speed** =
        TRUE. The first are estimated based on the distance they have
        travelled in the AIS data (the threshold of distance can be
        customized for advanced use), the latter are estimated based on
        their speed in the AIS (threshold can be customized as well).

-   Extract the positions of vessels, interpolated or not, at desired
    times and around desired locations, with the function
    **AISextract**. A parameter **search_into_radius_m** allows
    extracting vessel positions that are within a certain distance (in
    meters) from your input location(s).

    -   Your input data is a data frame where each row contains a
        combination of time\~location (location around which you want to
        extract vessel locations). You can, for example, define input
        data as your survey / bio-logging data.<br>
    -   The function parameter **t_gap** defines the time range over
        which AIS data are considered for extracting the vessel
        locations at the closest moment to your input times. Default is
        **t_gap** = 10×60 = 10 minutes, meaning AIS data at the input
        times ± 10 minutes are kept for extracting vessel locations at
        the closest time of your input time.<br>
    -   If you are also interested in the presence of vessels before
        your input time (e.g. how many vessels where present every
        minute in the study area, until 10 minutes before the survey
        took place): you can set **duplicate_time** = TRUE,
        **max_time_diff** = 10×60 (number of seconds) and **t_gap** =
        1×60 (number of seconds). This extracts the vessel locations
        every minutes from your input time until 10 minutes before, for
        later advanced analysis.<br>
    -   If you want to extract the vessel locations within an area that
        is not circular, it is recommended to enter, as input data,
        different locations that allow to cover your study area when
        they are overlapped by the radius of **search_into_radius_m**.
        Duplicated vessel locations must then be removed from the output
        data, and vessels signals that are within your study area must
        be filtered using sf intersection, for example. This is not too
        complicated, but do not hesitate to contact me for more
        information, and we could consider adding some options in the
        function to extract directly vessel locations within the
        non-circular areas, without these additional steps.<br>
    -   If many input times are defined for extraction, the process will
        also be long. The computation time can be greatly decreased with
        **accelerate** = TRUE and **average_at** (number of seconds).
        This average the input times that you want to extract to time
        intervals that are separated by ‘**average_at**’ number of
        seconds.<br>
    -   The output data are the input data, with the AIS data of
        surrounding vessels joined using left_join (dplyr R-package).
        This means that an input data with 2 surrounding vessels will be
        present 2 times in the output data, each row containing the AIS
        data of 1 of the vessel, at the closest moment of the time you
        want to extract. An input data without any surrounding vessel
        will be present 1 time in the ouput data, but the columns
        corresponding to the AIS data will all be filled by NA. <br>
    -   In addition to the columns of the input data and AIS data, the
        output data contains other columns that add information on the
        AIS data returned (for example, how many points this MMSI has in
        the AIS data; whether any **speed_kmh** value was NA (very
        unexpected !); or the difference of seconds between the input
        time and the extracted time). More information on these columns
        can be found typing **??AIStravel_interpolate_extract** or
        **??AISextract** on R console.

##### Parallelize:

**AISinterpolate_at** can be run in parallel (with **parallelize** =
TRUE; you can define the number of cores to use with nb_cores and the
file where iterations, warnings and errors are printed with outfile
(doParallel does not print results in the console of R by default).
outfile can also contain the path of the folder where you want to save
the file, in addition to the name of the file (usually in .txt
extension, must be specified in outfile).

##### Careful:

The RAM and CPU needed to run large datasets increases exponentially and
parallel should be run only after having investigated the RAM and CPU
used in series computation.

##### Important:

**Most of the time, you are interesting running the above 3 steps
(calculate speed, interpolate vessel tracks and extract vessel positions
in your study area at the desired times). To simplify the process, the
function AIStravel_interpolate_extract does the full process for you in
a single run**: you can enter your AIS data, your input data and define
wether you want to extract the presence of vessels prior to your input
data as well (**duplicate_time** = TRUE, **max_time_diff** = number of
seconds until which you are want to extract vessel locations, **t_gap**
= intervals of seconds over which vessel locations are extracted from
your input times until ‘max_time_diff’ seconds before). You can also
define your radius where vessels signals are considered for
interpolation (see **AISinterpolate_at** above), and
**search_into_radius_m** (distance over which you want to extract vessel
locations around your input locations). Due to the high computation time
for large datasets (either input or AIS), the function also allows to
progressively write the results during the process, avoiding to spend
extra time if the function crashes in reason of large computation
requirements. With the appropriate set of parameters, the function can
be simply re-run to load the saved results and to continue at the step
it stopped. Please do not used the files from **save_AISinterlate_at**
outside of the function as they are not complete: they need to be loaded
by the function **AISinterpolate_at** (or
**AIStravel_interpolate_extract**) to be completed. To do this, you have
the function parameters:

-   **Overwrite**: if TRUE, the function runs all the steps and
    overwrite the existing files.

-   **load_existing_files**: if TRUE, the function loads the existing
    files, when they exist (otherwise the function runs the usual
    process).

-   **save_AIStravel**, **save_AISinterlate_at**,
    **save_AISextract_perHour**: if TRUE, save the results of the
    functions during the process. For the functions **AISinterlate_at**
    and **AISextract**, the results are saved for every day and hour
    processed. The results are saved as .rds files. The file names can
    be chosen with the parameters **file_AIStravel**,
    **file_AISinterlate_at**, **file_AISextract_perHour** (do not
    specify file extension as .rds will be automatically added, but
    these names can contain the folder path where you want to save the
    results). If **load_existing_files** = TRUE, the function try to
    load the files named as the 3 above parameters (if they exist,
    otherwise it runs the full function as usual). Please do not used
    the files from **save_AISinterlate_at** outside of the function as
    they are not complete: they need to be re-loaded by the function.

-   **run_AIStravel**: if TRUE, run the function **AIStravel**. If the
    function has already been run and results saved but that the
    function stopped afterwards, you can define it as FALSE and
    **load_existing_files** = TRUE to reload the results and continue
    the calculation of interpolation/extraction.

-   **run_AISinterpolate_at**: same but for the interpolation of vessel
    tracks. If the function has stopped during **AISinterpolate_at**,
    and not during **AIStravel**, you can simply set
    **load_existing_files** = TRUE, **run_AIStravel** = FALSE and
    **run_AISinterpolate_at** = TRUE to run only the latter and skip the
    **AIStravel**.

-   **run_AISextract_perHour**: same but for the extraction of AIS data
    around your desired location\~time. If the function has stopped
    during **AISextract**, and not before, you can simply set
    **load_existing_files** = TRUE, **run_AIStravel** = FALSE,
    **run_AISinterpolate_at** = FALSE and **run_AISextract_perHour** =
    TRUE to run only the latter and skip the first steps.

#### 2. For further information:

-   **MMSI_infos**: taking AIS data as input, this function aims to
    estimate the most probable type, length, draught, width, imo and
    name of each vessel based on the AIS information. These can indeed
    contain diverging information for single vessels. The function
    returns a list of 2 elements:

    -   The first are the most probable **type**, **length**,
        **draught**, **width**, **imo** and **name** for each vessel
        (MMSI).

    -   The second are the detailed information that led to these
        estimates (type **??MMSI_infos** on R for detailed information
        on the columns).

The information on vessel length, type, and other, can then be joined by
MMSI to the results obtained with the above functions.

-   **AIScorrect_speed**: is the function that corrects GPS gaps and
    unrealistic speeds in the AIS data. It can be used outside of
    **AISinterpolate_at**.

    -   If you want to keep the high-speed crafts in output data and
        avoid correcting their speed, you can set
        **correct_high_speed_craft** to FALSE.

    -   A fixed upper threshold (in km/h) can be defined in parameters
        to define “unrealistic speed”.

    -   Another vessel-specific threshold can be defined as a function
        of the vessel speed. This function can use the argument
        **speed_kmh** (the vector of speed for each vessel) to use the
        mean, for example, of the vessel speed – you have to use
        mean(speed_kmh) in this case – and define a upper threshold
        adapted to the vessel. In the same manner, the median –
        median(speed_kmh) – or quantile – quantile(speed_kmh, 0.95) for
        example – can be used to define the upper threshold, as well as
        any value derived from the vessel speed. You can take a look to
        the default expression used for this threshold in
        **AIScorrect_speed** to see an example.
