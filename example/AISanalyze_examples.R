library(devtools)
install_github("https://github.com/remip48/AISanalyze.git", force = T)

# !!! For the moment, the functions are using ETRS3035 projection, adapted for the European zone.
# The package needs to be extended to use of other areas.

library(AISanalyze)

## example data
data("ais")
data("point_to_extract")

library(dplyr)
library(lubridate)

View(ais) # you may want to filter some MMSI by yourself, like these with less than 9 digits or starting by 1
# (not done in the above functions). Base-stations and aircraft are determined in the below function using
# the speed and the distance travelled. If more information is known by the user, she/he can first filter them.
View(point_to_extract)

## need to create the number of seconds since 1970 (or another date) for both AIS data and
# points to extract
ais <- ais %>%
  mutate(timestamp = as.numeric(ymd_hms(datetime)))

point_to_extract <- point_to_extract %>%
  mutate(timestamp = as.numeric(ymd_hm(datetime))) %>%
  dplyr::rename(id = point) # just a little corrections from my side.

ggplot() +
  geom_point(data = point_to_extract, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_point(data = ais, aes(x = lon, y = lat, color = timestamp), size = 1) +
  geom_point(data = point_to_extract, aes(x = lon, y = lat, color = timestamp), size = 1) +
  scale_color_viridis_c(labels = function(breaks) {as_datetime(breaks)}) +
  labs(title = "AIS data plotted with the points (red)\naround which we want to extract vessels.\nTimestamp is plotted for all points.")

## To calculate the distance and time travelled between each AIS signal per vessel, as well as their speed (km/h).
# The function also removes all latitudes = 0, being GPS errors.
AIStrav <- AIStravel(ais_data = ais,
                time_stop = 5*60*60, ## maximum interval of seconds between 2 vessel AIS signal over which a
                # vessel track is considered as continuous. If longer, the vessel track is divided in 2 distinct
                # tracks (before and after this interval). Here, 5 hours.
                mmsi_time_to_order = T, ## the MMSI and timestamps must be ordered. Better to let TRUE here
                # for automatic ordering
                return_sf = F, # if you want to return an sf object
                return_3035_coords = F # if you want to return the ETRS coordinates in your output dataframe
)

## to interpolate data, after having calculated the distance, time and speed of travel.
AIS_int <- AISinterpolate_at(data = point_to_extract,
                             ais_data = AIStrav,
                             mmsi_time_to_order = F,
                             radius = 200000, # radius around the locations of 'point_to_extract' where AIS data are considered
                             # for interpolation. Here, 200 km to take far AIS data in case of AIS gap in offshore areas.
                             time_stop = 5*60*60,## maximum interval of seconds between 2 vessel AIS signal over which a
                             # vessel track is considered as continuous. If longer, the vessel track is divided in 2 distinct
                             # tracks (before and after this interval). Here, 5 hours.
                             correct_speed = T, # to correct unrealistic speeds and GPS errors.
                             threshold_speed_to_correct = 100, # fixed threshold to define 'unrealistic speed'.
                             filter_station = T, # if you want to remove base-stations from the output.
                             filter_high_speed = T, # if you want to remove high speed aircraft from the output.
                             # the speed threshold to estimate that an MMSI is an aircraft is 110 km/h (can be modified
                             # with the parameter 'threshold_high_speed').
                             parallelize = F, # to parallelize the interpolations: careful with RAM and CPU, prior
                             # runs in series are advised to check for the computation requirements of the process.
                             nb_cores = 4, # if parallelize = T, number of threads to use.

                             ## for long data processing:
                             save_AISinterlate_at = T, # if you want to iteratively save interpolated AIS data during the process
                             load_existing_files = T, # if you want to reload the saved files in case the function has
                             # stopped during the process and that you are re-running it to get final results.
                             overwrite = F, # to overwrite the saved files.
                             file_AISinterlate_at = paste0(getwd(), "/", "AISinterpolate_at") # file to save them (can contain the folder path).
                             # do not add file extension as .rds will be added.
)

ggplot() +
  geom_point(data = point_to_extract, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_point(data = AIS_int, aes(x = lon, y = lat), color = "orange", size = 2) +
  geom_point(data = ais, aes(x = lon, y = lat, color = timestamp), size = 1) +
  geom_point(data = AIS_int, aes(x = lon, y = lat, color = timestamp), size = 1.25) +
  geom_point(data = point_to_extract, aes(x = lon, y = lat, color = timestamp), size = 1) +
  scale_color_viridis_c(labels = function(breaks) {as_datetime(breaks)}) +
  labs(title = "The interpolated vessel locations has been added\nand are surrounded by orange")

## for extracting on the locations / times to extract.
extracted <- AISextract(data = point_to_extract,
                        ais_data = AIS_int,
                        search_into_radius_m = 50000, # the vessels present in a radius of 50 km around the locations of
                        # 'point_to_extract' and at the timestamps defined in 'point_to_extract' are returned
                        duplicate_time = F, # if you want to return the vessels present before the timestamps of
                        # 'point_to_extract', set to TRUE
                        max_time_diff = 0, # if duplicate_time = TRUE, number of seconds before the timestamps of
                        # 'point_to_extract' where the vessels presence is returned.
                        t_gap = 10*60, # if duplicate_time = TRUE, time steps (seconds) over which the presence of boats is
                        # returned, from the 'point_to_extract' timestamps to 'max_time_diff' before. If
                        # duplicate_time = FALSE,
                        accelerate = T, # if you want to accelerate the process: see 'average_at' below:
                        average_at = 60 # if accelerate = TRUE, it averages the timestamps to extract (from
                        # both 'point_to_extract' and extended to the past by duplicate_time = TRUE) to
                        # interval of seconds separated by 'average_at' number of seconds. Then, if 2 timestamps
                        # are separated by only 1 second, for example, the function does not take double of time.
                        )

## this output correspond to the "point_to_extract" data frame, with close vessels of each line joined
# to the corresponding line. If 2 vessels are close from the location of one line, this line is duplicated
# twice (one for each vessel). If no vessels are nearby, the line is returned but the columns giving vessel
# information in the output are all filled by NA.

study_area <- point_to_extract %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_buffer(units::set_units(50, km)) %>%
  group_by() %>%
  dplyr::summarise() %>%
  st_cast("POLYGON")

ggplot() +
  geom_sf(data = study_area) +
  geom_point(data = point_to_extract, aes(x = lon, y = lat), color = "red", size = 2) +
  # geom_point(data = AIS_int, aes(x = lon, y = lat), color = "orange", size = 2) +
  geom_point(data = extracted, aes(x = ais_lon, y = ais_lat), color = "brown", size = 2) +
  geom_point(data = ais, aes(x = lon, y = lat, color = timestamp), size = 1) +
  # geom_point(data = AIS_int, aes(x = lon, y = lat, color = timestamp), size = 1.25) +
  geom_point(data = extracted, aes(x = ais_lon, y = ais_lat, color = timestamp), size = 1.25) +
  geom_point(data = point_to_extract, aes(x = lon, y = lat, color = timestamp), size = 1) +
  # facet_wrap(~timestamp_AIS_to_extract) +
  scale_color_viridis_c(labels = function(breaks) {as_datetime(breaks)}) +
  coord_sf(xlim = st_bbox(study_area)[c(1, 3)],
           ylim = st_bbox(study_area)[c(2, 4)]) +
  labs(title = "Study area with extracted points in brown\n(from timestamps to extract to 1 hour before\nby steps of 1 min)")

ggplot() +
  geom_point(data = extracted, aes(x = ais_lon, y = ais_lat, color = timestamp)) +
  scale_color_viridis_c(labels = function(breaks) {as_datetime(breaks)})

## new columns present in the output (i.e. other than 'point_to_extract' columns):
# actually most of them are columns from AIS data, that were merged to the 'point_to_extract' data if
# vessels were closed by.
colnames(extracted)[!(colnames(extracted) %in% colnames(point_to_extract))]
## columns that are neither in 'point_to_extract' and AIS data:
# actually columns starting by 'ais_' below (e.g. ais_timestamp, ais_lat) are columns from the AIS but were
# renamed due to conflicting names with 'point_to_extract' columns. Other columns were created during the 3
# above functions to give information on the vessel either on their travel, interpolation, or extraction.
colnames(extracted)[!(colnames(extracted) %in% c(colnames(point_to_extract),
                                                 colnames(ais)))]

############################## ONE FOR ALL
## Most of the time, you are interested by running AIStravel, AISinterpolate_at and AISextract to directly
# extract the vessel presence around your datapoints, at their timestamps (and possibly prior as well) and
# knowing the vessel speed. Then, you can run the function below which will run the 3 functions for
# you, and save the results iteratively if you wish:
all_in_one <- AIStravel_interpolate_extract(data = point_to_extract,
                                            ais_data = ais,

                                            # you find below the same parameters than in the 3 functions above:
                                            mmsi_time_to_order = T,
                                            search_into_radius_m = 50000,
                                            correct_speed = T,
                                            threshold_speed_to_correct = 100,

                                            duplicate_time = F,
                                            max_time_diff = 0,
                                            t_gap = 10*60,
                                            accelerate = F,
                                            average_at = 0,
                                            filter_station = T,
                                            filter_high_speed = T,
                                            radius = 200000,
                                            time_stop = 5*60*60,
                                            parallelize = F,
                                            nb_cores = NA,

                                            run_AIStravel = T, # do you want to run AIStravel (if not done yet)
                                            save_AIStravel = F,
                                            file_AIStravel = paste0(getwd(), "/", "AIStravel"), # name to save the results
                                            # from AIStravel: might contain the folder path (but not the file extension
                                            # as .rds will be added !)

                                            run_AISinterpolate_at = T, # do you want to run AISinterpolate_at (if not done yet)
                                            save_AISinterlate_at = F, # (saved for every hour interpolated)
                                            file_AISinterlate_at = paste0(getwd(), "/", "AISinterpolate_at"),

                                            run_AISextract_perHour = T, # do you want to run AISextract (if not done yet)
                                            save_AISextract_perHour = F, # (saved for every hour extracted)
                                            file_AISextract_perHour = paste0(getwd(), "/", "AISextract"),

                                            overwrite = F,
                                            load_existing_files = F,

                                            return_merged_all_extracted = T # do you need to return all the extracted
                                            # vessel locations associated with the points to extract ? This might
                                            # be very large data in some cases, and you might be interested to not
                                            # return the full dataset to take a look, hour by hour, to the saved files.

)

## to extract information on MMSI length, type, draught, etc, based on AIS data:
infos <- MMSI_infos(ais_data = ais,
                    threshold_length = 475,
                    weight_complete_data = 10)

# this returns two elements: a dataframe with the information per MMSI, and a dataframe with all the information
# collected in the AIS data (for example, all the 'length' value found for single vessels, how many points
# had a 'length' value, if more than one 'length' value was found for a single vessel, etc). You can take a look
# to the help page to know more about the returned columns.

View(infos)
View(infos$values_obtained)
View(infos$summary)
