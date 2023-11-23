########################################################################## LIBRARY ###########################################################################
library(terra)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(landscapemetrics)
library(sf)
library(tigris)
library(viridis)
library(tidycensus)
library(patchwork)
library(scales)
options(tigris_use_cache = TRUE)


rm(list=ls())

########################################################################### NOTES #############################################################################

# PURPOSE OF THIS FILE
# The point of this file is to calculate the landscape metrics, then save the dataframe as an .Rda file
# This way, we do not have to re-calculate the lm every time we want to use them.

# NOTES FOR CLEANING GOLF COURSE DATA
# EDITED GOLF COURSE POLYGONS (first round)
# tuscany east and west are the same polygon --36 holes into one. [DONE]
# willowcreek regulation and par3 course are the same polygon. --36 into one.[DONE]
# sun city east and west 36 into one [DONE]
# desert mountain [chiricahua, cochise, geronemo] [DONE]
# whisper rock [upper and lower] [DONE]
# wildfire [faldo and palmer] [DONE]
# troon north pinnacle and monument [DONE]
# biltmore [links, adobe] [DONE]
# superstition mountain country club [prospector, lost gold] [PINAL]
# gold canyon [dinosaur, sidewinder] [PINAL]
# boulders (north + south) [DONE]
# oakwood (3 nine-hole courses)

# EDITED GOLF COURSE POLYGONS (second round)
# scottsdale national (gc_id = 126) is counted right now as one patch. In reality is it 2 18-hole courses (Mine shaft course; Other Course; Bad Little 9) (Polygons need to be split). [DONE]
# tonto verde (gc_id = 153) is now labelled as the same course; however it is actually 2 courses, the Peaks and Ranch course. (Polygons need to be split). [Done]
# talking stick (gc_id = 149) is now labelled as one patch; however it is actually the Piipaash and O'odham course. (Polygons need to be split). [Done]
# wekopa (gc_id = 170) has the saguaro and the cholla course. (polygons need to be split) [Done]
# bear creek golf course needs to be changed to the bear and cub course (polygon split needed) [Done]
# wildhorse cattail course (gc_id = 180) needs to be rennamed to devils claw. The smaller devil's claw polygon needs to be split (Polygon split). [Done]
# renegade golf course of desert mountain (gc_id = 43) -- needs the knob on the end of it cut off (Polygon split).
# Palm valley golf course (gc_id = 101) is currently labelled as two polygons; in reality palm valley is 3 9-hole courses (North, South, West). One option (best option is to split the polygons) (Polygon Split.) [Done]
# the wigwam course (gc_id = 177) is actually two courses: The Gold and Blue Course (see scorecards on the website). (Polygons need to be split)
# aguila 9 (gc_id = 2) needs to be recognized from the aguila 18 (polygons need to be split) [Done]
# encanto (gc_id = 53) needs to be split from 27 to encanto_9, and encanto_18 (polygons need to be split) [DONE]
# gainey ranch (gc_id = 62) needs to be split into 3 9-hole golf courses (lakes, dunes, arroyo courses) (polygons need to be split)
# westin kierland (gc_id = 173) has 3 9-hole courses (mesquite, acacia, ironwood) (polygons need to be split) [Done]
# sunland springs (gc_id = 145) is actually 3 9-hole courses (four_peaks, superstition, san tan) [Done]

# litchfield greens (gc_id == ?) is actually the wigwam red course [DONE]
# palo verde golf course (9-hole course next to Phoenix country club) is currently labelled as "palo verde" (change name to palo_verde_golf_course)
# palo verde country club (down in chandler) is currently labelled as "palo verde" --get rid of patch on the far right (change name to palo_verde_country_club)
# Copper Canyon golf course (gc_id = 26) has a Vista, Lake, and Mountain 9-hole course. The attributes for this patch need to be changed.
# The patch labelled as "the Raven" (gc_id = 118) that is 178.88 acres is actually Legacy Golf Club. The attributes for that patch need to be changed.
# "Dome valley ranch" needs to be changed to "Rancho Manana" golf course.
# pueblo el mirae needs to be changed to "Pueblo El Mirage"
# anthem country club needs to have a course added "persimmon course"
# ironwood country club needs to be changed to and them country club and have "ironwood course" added under course.

# 9-hole courses
# paradise peak west course (gc_id = 105)
# royal palms (gc_id = 123)
# stripe show golf club (gc_id = 134)
# sun village golf course (gc_id = 143)
# shalimar (gc_id = 129)
# desert mirage (37)
# coronado (gc_id = 27)
# oakwood golf club courses (Lakes, Sonoran, Palms)
# Willowcreek executive 
# quail run (gc_id = 115)
# Bad little 9 (scottsdale national; see above)
# palm valley golf course
# copper canyon golf course
# encanto 9
# aguila 9
# gainey ranch
# westin kierland
# sunland springs
# palo verde golf course



####################################################################### WORKING DIRECTORY ##################################################

# TODO insert the path to your working directory here:
# setwd("")

######################################################################## LOAD LUCC DATA ##########################################################################

gc_status <- read_csv("data/gc_status.csv")

# cleaning the original MAG data.
# "delete" was assigned to polygons that are extremely small (<1 acre) and attached to other golf course polygons (filter them out)
# "delete" was also assigned to polygons that are abandoned golf courses turned into residential (filter them out)
#  dead golf course is assigned to golf courses that are no longer being maintained
# "driving range" is assigned to polygons that are only driving ranges--not golf courses (filter them out)

# load and clean the polygon data
gc_polygons_sf <- st_read("data/gc_spatial_data/cleaned_gc_spatial_data.shp") %>%  
  select(-gc_id) %>% 
  filter(gc_name != "random_polyon") %>% 
  mutate(course = case_when((gc_name == "scottsdale_country_club" & ACRES > 100) ~ "king_course",
                            (gc_name == "scottsdale_country_club" & ACRES < 100) ~ "mulligan_course",
                            .default =  course)) %>% 
  mutate(course = case_when(gc_name == "lakes_course" ~ "lakes_course",
                            gc_name == "westbrook_village_golf_club" ~ "vistas_course",
                            gc_name == "tpc_scottsdale" ~ "stadium_course",
                            gc_name == "tpc_scottsdale_champions" ~ "champions_course", 
                            gc_name == "coyote_run" ~ "coyote_run",
                            gc_name == "heron_lakes" ~ "heron_lakes",
                            .default =  course)) %>% 
  mutate(gc_name = case_when(course == "coyote_run" ~ "leisure_world_golf_course",
                             course == "heron_lakes" ~ "leisure_world_golf_course",
                             .default = gc_name)) %>% 
  mutate(gc_name = case_when((gc_name == "sun_city_golf_course" & course == "north_course") ~ "scr_north_gc",
                             (gc_name == "sun_city_golf_course" & course == "south_course") ~ "scr_south_gc",
                             (gc_name == "sun_city_golf_course" & course == "east") ~ "scr_east_gc",
                             (gc_name == "sun_city_golf_course" & course == "west") ~ "scr_west_gc",
                             .default = gc_name)) %>% 
  mutate(course = case_when((gc_name == "scr_north_gc") ~ NA, # does this introduce a different type of NA value?
                            (gc_name == "scr_south_gc") ~ NA,
                            (gc_name == "scr_east_gc") ~ NA,
                            (gc_name == "scr_west_gc") ~ NA,
                            .default = course)) %>% 
  mutate(gc_name = case_when(gc_name == "ingleside" ~ "arizona_country_club",
                             gc_name == "happy_trails_golf_club" ~ "great_eagle_golf_club",
                             gc_name == "kokopeli_golf_club" ~ "kokopelli_gc",
                             gc_name == "falcon_dunes_golf_course" ~ "sterling_grove_country_club",
                             gc_name == "lakes_course" ~ "westbrook_village_golf_club",
                             gc_name == "scottsdale_country_club" ~ "starfire_golf_club",
                             gc_name == "tpc_scottsdale_champions" ~ "tpc_scottsdale",
                             gc_name == "wigwam" ~ "wigwam_golf_club",
                             gc_name == "ahwatukee_counry_club" ~ "ahwatukee_country_club",
                             .default =  gc_name)) %>% 
  mutate(gc_name = case_when(gc_name == "adobe_dam" ~ "the_500_club",
                             gc_name == "aguila_golf_course" ~ "aguila_golf_course",
                             gc_name == "ahwatukee_country_club" ~ "ahwatukee_country_club", 
                             gc_name == "alta_mesa" ~ "alta_mesa_country_club",
                             gc_name == "ancala_country_club" ~ "ancala_gc",
                             gc_name == "anthem_country_club" ~ "anthem_golf_and_country_club",
                             gc_name == "apache_wells" ~ "apache_wells_country_club",
                             gc_name == "arizona_country_club" ~ "arizona_country_club",
                             gc_name == "arizona_golf_resort" ~ "arizona_golf_resort",
                             gc_name == "arizona_grand" ~ "phantom_horse_gc",
                             gc_name == "arrow_head_country_club" ~ "arrowhead_country_club",
                             gc_name == "augusta_ranch" ~ "augusta_ranch_gc",
                             gc_name == "bear_creek" ~ "bear_creek_gc",
                             gc_name == "bellair" ~ "bellair_golf_club",
                             gc_name == "biltmore"~"arizona_biltmore_cc",
                             gc_name == "blackstone_country_club" ~ "blackstone_country_club",
                             gc_name == "boulders_golf_club" ~ "the_boulders_golf_club",
                             gc_name == "briarwood" ~ "briarwood_country_club",
                             gc_name == "camelback" ~ "camelback_gc",
                             gc_name == "cave_creek" ~ "cave_creek_gc",
                             gc_name == "cimarron_golf_course" ~ "cimmaron_gc",
                             gc_name == "coldwater_golf_club" ~ "coldwater_springs_golf_club",
                             gc_name == "continental" ~ "continental_golf_course",
                             gc_name == "copper_canyon_golf_club" ~ "copper_canyon_golf_club",
                             gc_name == "coronado" ~ "coronado_golf_course",
                             gc_name == "corte_bella_golf_course" ~ "corte_bella_gc",
                             gc_name == "cottonwood_country_club" ~ "cottonwood_country_club",
                             gc_name == "coyote_lakes" ~ "coyote_lakes",
                             gc_name == "dc_ranch" ~ "dc_ranch_country_club",
                             gc_name == "deer_valley_golf_course" ~ "scw_deer_valley",
                             gc_name == "desert_canyon_golf_club" ~ "desert_canyon_golf_club",
                             gc_name == "desert_forest_golf_club" ~ "desert_forest_gc",
                             gc_name == "desert_highlands" ~ "desert_highlands_gc",
                             gc_name == "desert_mirage_golf_and_practice_center" ~ "desert_mirage_gc",
                             gc_name == "desert_mountain" ~ "desert_mountain_golf_course",
                             gc_name == "desert_sands_golf_course" ~ "desert_sands_gc",
                             gc_name == "desert_springs_golf_course" ~ "scg_desert_springs",
                             gc_name == "desert_trails_golf_course" ~ "scw_desert_trails",
                             gc_name == "dobson_ranch" ~ "dobson_ranch_gc",
                             gc_name == "dove_valley_ranch" ~ "dove_valley_gc",
                             gc_name == "eagle_mountain" ~ "eagle_mountain_golf_club",
                             gc_name == "eagle_nest_golf_club" ~ "eagles_nest_golf_club",
                             gc_name == "echo_mesa_golf_course" ~ "scw_echo_mesa",
                             gc_name == "encanto" ~ "encanto_golf_course",
                             gc_name == "estancia" ~ "estancia_golf_club",
                             gc_name == "estrella_golf_club" ~ "estrella_golf_club",
                             gc_name == "fairway_hills" ~ "club_west_gc",
                             gc_name == "falcon_dunes" ~ "luke_falcon_dunes_gc",
                             gc_name == "fire_rock_country_club" ~ "fire_rock_gc",
                             gc_name == "foothills" ~ "foothills_gc",
                             gc_name == "fountain_of_the_sun" ~ "fountain_of_the_sun_gc",
                             gc_name == "gainey_ranch" ~ "gainey_ranch_golf_club",
                             gc_name == "grand_canyon_university" ~ "grand_canyon_university_golf_course",
                             gc_name == "grandview_golf_course" ~ "scw_grandview",
                             gc_name == "granite_falls_golf_club" ~ "scg_granite_falls",
                             gc_name == "grayhawk" ~ "grayhawk_golf_club",
                             gc_name == "greenfield_lakes" ~ "greenfield_lakes_gc",
                             gc_name == "great_eagle_golf_club" ~ "great_eagle_golf_club",
                             gc_name == "hillcrest" ~ "hillcrest_golf_club",
                             gc_name == "ironwood" ~ "sun_lakes_ironwood",
                             gc_name == "ken_mcdonald" ~ "ken_mcdonald_gc",
                             gc_name == "kokopelli_gc" ~ "kokopelli_gc",
                             gc_name == "las_colinas" ~ "las_colinas_golf_course",
                             gc_name == "las_sendas" ~ "las_sendas",
                             gc_name == "legacy" ~ "the_legacy_golf_resort",
                             gc_name == "legend_at_arrowhead" ~ "the_legend_at_arrowhead",
                             gc_name == "legend_trail" ~ "legend_trail_golf_course",
                             gc_name == "leisure_world_golf_course" ~ "leisure_world_golf_course",
                             gc_name == "lone_tree" ~ "lone_tree_golf_course",
                             gc_name == "longbow" ~ "longbow_golf_course",
                             gc_name == "lookout_mountain" ~ "pointe_on_lookout_mtn_gc",
                             gc_name == "los_caballeros_golf_club" ~ "los_caballeros_golf_club",
                             gc_name == "mccormick_ranch" ~ "mccormick_ranch_gc",
                             gc_name == "mcdowell_mountain" ~ "mcdowell_mountain",
                             gc_name == "mesa_country_club" ~ "mesa_country_club",
                             gc_name == "mirabel" ~ "the_mirabel_golf_club",
                             gc_name == "moon_valley" ~ "moon_valley_cc",
                             gc_name == "mountain_shadow" ~ "mountain_shadows_country_club",
                             gc_name == "oakwood" ~ "sun_lakes_oakwood_gc",
                             gc_name == "ocotillo" ~ "ocotillo_gc",
                             gc_name == "orange_tree" ~ "orange_tree_resort",
                             gc_name == "painted_mountain" ~ "painted_mountain_gc",
                             gc_name == "palm_valley_golf_club" ~ "palm_valley_golf_course",
                             gc_name == "palmbrook_country_club" ~ "palmbrook_country_club",
                             gc_name == "palo_verde_country_club" ~ "palo_verde_cc",
                             gc_name == "palo_verde_golf_course" ~ "palo_verde_gc",
                             gc_name == "papago" ~ "papago_gc",
                             gc_name == "paradise_peak_west" ~ "paradise_peak_gc",
                             gc_name == "paradise_valley" ~ "paradise_valley_gc",
                             gc_name == "paradise_valley_country_club" ~ "paradise_valley_country_club",
                             gc_name == "pebblebrook" ~ "scw_pebblebrook_gc",
                             gc_name == "peoria_pines" ~ "peoria_pines",
                             gc_name == "phoenician" ~ "the_phoenician_resort",
                             gc_name == "phoenix_country_club" ~ "phoenix_country_club",
                             gc_name == "pinnacle_peak_country_club" ~ "pinnacle_peak_cc",
                             gc_name == "power_ranch" ~ "trilogy_golf_course_at_power_ranch",
                             gc_name == "pueblo_el_mirage" ~ "pueblo_el_mirage_cc",
                             gc_name == "quail_run_golf_course" ~ "scr_quail_run",
                             gc_name == "quintero_golf_club" ~ "quintero_country_club",
                             gc_name == "rancho_manana" ~ "rancho_manana_golf_club",
                             gc_name == "raven" ~ "raven_golf_club",
                             gc_name == "red_mountain_ranch" ~ "red_mountain_ranch_gc",
                             gc_name == "rio_verde" ~ "rio_verde_gc_assoc",
                             gc_name == "riverview_golf_course" ~ "scr_riverview",
                             gc_name == "rolling_hills" ~ "rolling_hills_gc",
                             gc_name == "royal_palms_golf_course" ~ "royal_palms_gc",
                             gc_name == "san_marcos" ~ "san_marcos_resort",
                             gc_name == "scottsdale_national" ~ "the_golf_club_of_scottsdale",
                             gc_name == "scottsdale_silverado" ~ "silverado_golf_club",
                             gc_name == "seville" ~ "seville_golf_course",
                             gc_name == "shalimar" ~ "shalimar_country_club",
                             gc_name == "silverleaf" ~ "silverleaf_country_club",
                             gc_name == "springfield" ~ "springfield_golf_course",
                             gc_name == "stardust" ~ "scw_stardust",
                             gc_name == "starfire_golf_club" ~ "starfire_golf_club",
                             gc_name == "sterling_grove_country_club" ~ "sterling_grove_country_club",
                             gc_name == "stonecreek" ~ "stonecreek_gc",
                             gc_name == "stripe_show_golf_club" ~ "fiesta_lakes_gc",
                             gc_name == "sun_bird" ~ "sunbird_gc",
                             gc_name == "sun_city_country_club" ~ "sun_city_country_club",
                             gc_name == "sun_city_east_gc" ~ "scr_east_gc",
                             gc_name == "sun_city_west_gc" ~ "scr_west_gc",
                             gc_name == "sun_city_north_gc" ~ "scr_north_gc",
                             gc_name == "sun_city_south_gc" ~ "scr_south_gc",
                             gc_name == "sun_lakes" ~ "sun_lakes_country_club_hoa1",
                             gc_name == "sun_ridge_canyon" ~ "sun_ridge_canyon_gc",
                             gc_name == "sun_village_golf_course" ~ "sun_village_resort",
                             gc_name == "sundance_golf_club" ~ "sundance_golf_course",
                             gc_name == "sunland_springs" ~ "sunland_springs_gc",
                             gc_name == "sunland_village" ~ "sunland_village_gc",
                             gc_name == "sunland_village_east" ~ "sunland_village_east_gc",
                             gc_name == "superstition_springs" ~ "superstition_springs_gc",
                             gc_name == "talking_stick" ~ "talking_stick",
                             gc_name == "tatum_ranch" ~ "tatum_ranch_gc",
                             gc_name == "terravita" ~ "terravita_golf_and_country_club",
                             gc_name == "toka_sticks" ~ "toka_sticks",
                             gc_name == "tonto_verde" ~ "tonto_verde_gc",
                             gc_name == "tpc_scottsdale" ~ "tpc_scottsdale",
                             gc_name == "traditions_golf_club" ~ "arizona_traditions_gc",
                             gc_name == "trail_ridge_golf_course" ~ "scw_trailridge",
                             gc_name == "tres_rios_golf_club" ~ "tres_rios_golf_course",
                             gc_name == "trilogy_golf_club" ~ "trilogy_golf_club_at_vistancia",
                             gc_name == "troon_country_club" ~ "troon_village_gc",
                             gc_name == "troon_north" ~ "troon_north",
                             gc_name == "tuscany_falls_golf_course" ~ "tuscany_falls",
                             gc_name == "union_hills_country_club" ~ "union_hills_cc",
                             gc_name == "verde_river" ~ "vista_verde_gc",
                             gc_name == "verrado_golf_club" ~ "verrado_golf_club",
                             gc_name == "viewpoint" ~ "view_point_rv_and_golf_resort",
                             gc_name == "wekopa_golf_club" ~ "wekopa_golf_club",
                             gc_name == "westbrook_village_golf_club" ~ "westbrook_village_vistas_golf_club",
                             gc_name == "western_skies" ~ "western_skies_gc",
                             gc_name == "westin_kierland" ~ "kierland_gc",
                             gc_name == "whisper_rock" ~ "whisper_rock_gc",
                             gc_name == "wickenburg_golf_club" ~ "wickenburg_golf_club",
                             gc_name == "wigwam_golf_club" ~ "wigwam_golf_and_cc",
                             gc_name == "wildfire_golf_club" ~ "wildfire_gc",
                             gc_name == "wildhorse" ~ "whirlwind_at_wildhorse_pass",
                             gc_name == "willow_creek" ~ "scr_willowcreek",
                             .default =  gc_name)) %>%
  mutate(num_holes = case_when((gc_name == "aguila_golf_course" & course == "aguila_9") ~ 9,
                               (gc_name == "coronado_golf_course") ~ 9,
                               (gc_name == "copper_canyon_golf_club" & course == "mountain") ~ 9,
                               (gc_name == "copper_canyon_golf_club" & course == "vista") ~ 9,
                               (gc_name == "copper_canyon_golf_club" & course == "lake") ~ 9,
                               (gc_name == "desert_mirage_gc") ~ 9,
                               (gc_name == "encanto_golf_course" & course == "encanto_9") ~ 9,
                               (gc_name == "gainey_ranch_golf_club" & course == "arroyo") ~ 9,
                               (gc_name == "gainey_ranch_golf_club" & course == "lake") ~ 9,
                               (gc_name == "gainey_ranch_golf_club" & course == "dunes") ~ 9,
                               (gc_name == "kierland_gc" & course == "mesquite") ~ 9,
                               (gc_name == "kierland_gc" & course == "ironwood") ~ 9,
                               (gc_name == "kierland_gc" & course == "acacia") ~ 9,
                               (gc_name == "sun_lakes_oakwood_gc" & course == "sonoran") ~ 9,
                               (gc_name == "sun_lakes_oakwood_gc" & course == "palms") ~ 9,
                               (gc_name == "sun_lakes_oakwood_gc" & course == "lakes") ~ 9,
                               (gc_name == "ocotillo_gc" & course == "white") ~ 9,
                               (gc_name == "ocotillo_gc" & course == "yellow") ~ 9,
                               (gc_name == "ocotillo_gc" & course == "blue") ~ 9,
                               (gc_name == "sun_lakes_oakwood_gc" & course == "lakes") ~ 9,
                               (gc_name == "sun_lakes_oakwood_gc" & course == "lakes") ~ 9,
                               (gc_name == "palm_valley_golf_course" & course == "west_course") ~ 9,
                               (gc_name == "palm_valley_golf_course" & course == "south_course") ~ 9,
                               (gc_name == "palm_valley_golf_course" & course == "north_course") ~ 9,
                               (gc_name == "palo_verde_gc") ~ 9,
                               (gc_name == "paradise_peak_west") ~ 9,
                               (gc_name == "scr_quail_run") ~ 9,
                               (gc_name == "royal_palms_gc") ~ 9,
                               (gc_name == "fiesta_lakes_gc") ~ 9,
                               (gc_name == "the_golf_club_of_scottsdale" & course == "bad_little_nine") ~ 9,
                               (gc_name == "sunland_springs_gc" & course == "superstition_course") ~ 9,
                               (gc_name == "sunland_springs_gc" & course == "santan_course") ~ 9, 
                               (gc_name == "sunland_springs_gc" & course == "four_peaks_course") ~ 9,
                               (gc_name == "shalimar_country_club") ~ 9,
                               (gc_name == "scr_willowcreek"  & course == "executive") ~ 9,
                               (gc_name == "starfire_golf_club"  & course == "mulligan_course") ~ 9, 
                               (gc_name == "view_point_rv_and_golf_resort"  & course == "executive") ~ 9,
                               .default = 18), .before = ACRES) %>% 
  mutate(gc_id = group_indices(., gc_name, course), .before = ACRES) %>% 
  mutate(gc_name_id = group_indices(., gc_name), .before = ACRES) %>% 
  mutate(unique_patch_id = row_number(), .before = ACRES) %>% 
  merge(., gc_status, by = "gc_name")

saveRDS(gc_polygons_sf, file= "data/rda_files/gc_polygons_sf.Rda")


gc_polygons <- gc_polygons_sf %>% 
  vect()

#### . ####
########################################################################### golf course scale analysis #########################################################################


# Each golf course is defined as its own class. Golf course is defined according to "gc_id", which is based on "gc_name" and "course". 
# The landscape metrics calculated here make up the golf course scale analysis referred to in the paper.

#### @rasterize the GC data ####
r <- rast(gc_polygons, ncols=8400, nrows=6850)
gc_15m <- rasterize(gc_polygons, r, "gc_id")
gc_15m

r <- rast(gc_polygons, ncols=4463, nrows=3457)
gc_30m <- rasterize(gc_polygons, r, "gc_id")
gc_30m

levels(gc_30m)
cats(gc_30m)

# because the column 'gc_id' is not a factor, we cannot see the values assigned to each 'gc_id'.
# I am sure that the gc_id is lined up with pixel value. The class column in the output after landscape metrics are calculated must line up with the gc_id column.

#### @test detail ####

# large sun city Extent
e_sc_large <- ext(-12520000, -12480000, 3960000, 3990000)
# Sun City Subset Extent
e_sc_small <- ext(-12520000, -12510000, 3980000, 3988000)

png('photos/lm_calculations_EDA/15m_resolution_scl.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_15m, e_sc_large))
dev.off()

png('photos/lm_calculations_EDA/15m_resolution_scs.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_15m, e_sc_small))
dev.off()

png('photos/lm_calculations_EDA/30m_resolution_scl.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_30m, e_sc_large))
dev.off()

png('photos/lm_calculations_EDA/30m_resolution_scs.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_30m, e_sc_small))
dev.off()

#### @reclassify ####

# check number of golf courses
gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  View()

# This way, we may be changing the extent to do the golf course landscape analysis. If we continue to do it this way, then we will need to select landscape metrics that are not influenced by the 
# extent of the landscape just to be safe.. Another alternative is to treat each golf course as its own landscape. Determining the extent for each landscape would be a challenge.

# to calculate the follow values for each golf course, we are going to have to do 30 golf courses at a time (see check_landscape function)
# In total, we have 210 golf courses--therefore, we need to split the data into 7 different vect() objects. 
# first, filter each df to include ~30 golf courses (30 classes)
# the reason we have to do a reclassification is because we cannot just filter the dataframe into groups of courses. That approach does not allow us to keep the resolution constant.
# lsm_c_lsi, lsm_c_ai, and lsm_c_lpi are impervious to changing extent size.

# reclassification 1 (keep courses 1-30)
x1 <- 31:210
x2 <- rep(NA, 180) #x2 should be the same for all other vectors

rclmat_1 <- cbind(x1,x2)
gc_1.30 <- classify(gc_30m, rclmat_1)

# reclassification 2 (keep courses 31-60)
v1 <- 1:30
v2 <- 61:210
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_31.60 <- classify(gc_30m, rclmat_1)

# reclassification 3 (keep courses 61-90)
v1 <- 1:60
v2 <- 91:210
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_61.90 <- classify(gc_30m, rclmat_1)

# reclassification 4 (keep courses 91-120)
v1 <- 1:90
v2 <- 121:210
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_91.120 <- classify(gc_30m, rclmat_1)

# reclassification 5 (keep courses 121-150)
v1 <- 1:120
v2 <- 151:210
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_121.150 <- classify(gc_30m, rclmat_1)

# reclassification 6 (keep courses 151-180)
v1 <- 1:150
v2 <- 181:210
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_151.180 <- classify(gc_30m, rclmat_1)

# reclassification 7 (keep courses 181-210)
x1 <- 1:180
x2 <- rep(NA, 180)

rclmat_1 <- cbind(x1,x2)
gc_181.210 <- classify(gc_30m, rclmat_1)

# test each reclassified raster
plot(gc_1.30)
plot(gc_31.60)
plot(gc_61.90)
plot(gc_91.120)
plot(gc_121.150)
plot(gc_151.180)
plot(gc_181.210)

#### @calculate_lsm ####

gc_1.30_lm <- calculate_lsm(gc_1.30, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_31.60_lm <- calculate_lsm(gc_31.60, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_61.90_lm <- calculate_lsm(gc_61.90, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_91.120_lm <- calculate_lsm(gc_91.120, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_121.150_lm <- calculate_lsm(gc_121.150, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_151.180_lm <- calculate_lsm(gc_151.180, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_181.210_lm <- calculate_lsm(gc_181.210, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))

golf_course_scale_metrics <- gc_1.30_lm %>% 
  rbind(gc_31.60_lm) %>% 
  rbind(gc_61.90_lm) %>% 
  rbind(gc_91.120_lm) %>% 
  rbind(gc_121.150_lm) %>% 
  rbind(gc_151.180_lm) %>% 
  rbind(gc_181.210_lm)

saveRDS(golf_course_scale_metrics, file= "data/rda_files/golf_course_scale_metrics.Rda")

#### . ####
########################################################################### golf club scale analysis #########################################################################

# Each golf club is defined as its own class. Golf course is defined according to "gc_name_id".

#### @rasterize the GC data ####
r <- rast(gc_polygons, ncols=8400, nrows=6850)
gc_15m <- rasterize(gc_polygons, r, "gc_name_id")
gc_15m

r <- rast(gc_polygons, ncols=4463, nrows=3457)
gc_30m <- rasterize(gc_polygons, r, "gc_name_id")
gc_30m
levels(gc_30m)
cats(gc_30m)


#### @reclassify ####

# check number of golf courses
gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_name) %>% 
  unique() %>% 
  View()

# reclassification 1 (keep courses 1-30)
# The value of 0 is assigned to the first golf course name.
x1 <- 31:159
x2 <- rep(NA, 129) #x2 should be the same for all other vectors

rclmat_1 <- cbind(x1,x2)
gc_1.30 <- classify(gc_30m, rclmat_1)

# reclassification 2 (keep courses 31-60)
v1 <- 1:30
v2 <- 61:159
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_31.60 <- classify(gc_30m, rclmat_1)

# reclassification 3 (keep courses 61-90)
v1 <- 1:60
v2 <- 91:159
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_61.90 <- classify(gc_30m, rclmat_1)

# reclassification 4 (keep courses 91-120)
v1 <- 1:90
v2 <- 121:159
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_91.120 <- classify(gc_30m, rclmat_1)

# reclassification 5 (keep courses 121-150)
v1 <- 1:120
v2 <- 151:159
x1 <- c(v1,v2)

rclmat_1 <- cbind(x1,x2)
gc_121.150 <- classify(gc_30m, rclmat_1)

plot(gc_121.150)

# reclassification 6 (keep courses 151-159)
x1 <- 1:150
x2 <- rep(NA, 150)

rclmat_1 <- cbind(x1,x2)
gc_151.159 <- classify(gc_30m, rclmat_1)

# test each reclassified raster
plot(gc_1.30)
plot(gc_31.60)
plot(gc_61.90)
plot(gc_91.120)
plot(gc_121.150)
plot(gc_151.159)

#### @calculate_lsm ####

gc_1.30_lm <- calculate_lsm(gc_1.30, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_31.60_lm <- calculate_lsm(gc_31.60, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_61.90_lm <- calculate_lsm(gc_61.90, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_91.120_lm <- calculate_lsm(gc_91.120, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_121.150_lm <- calculate_lsm(gc_121.150, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))
gc_151.159_lm <- calculate_lsm(gc_151.159, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_p_area"))

golf_club_scale_metrics <- gc_1.30_lm %>% 
  rbind(gc_31.60_lm) %>% 
  rbind(gc_61.90_lm) %>% 
  rbind(gc_91.120_lm) %>% 
  rbind(gc_121.150_lm) %>% 
  rbind(gc_151.159_lm) 

saveRDS(golf_club_scale_metrics, file= "data/rda_files/golf_club_scale_metrics.Rda")




#### . ####
########################################################################### golf course class analysis #########################################################################

# For this analysis, each golf course patch is reclassified to the same patch. Thus, all golf course patches are classfied the same.
# The patch-scale analysis and the landscape-scale analysis are contained within the golf course class scale analysis

# reclassify gc_cleaned to one class (golf course)
# reclassification (all courses to the same class)
x1 <- 1:210
x2 <- rep(1, 210)

rclmat_1 <- cbind(x1,x2)
gc_class_level_30m <- classify(gc_30m, rclmat_1)

check_landscape(gc_class_level_30m)
gc_patch_landscape_scale_metrics <- calculate_lsm(gc_class_level_30m, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_pd", "lsm_c_lsi", "lsm_c_ai", "lsm_p_cai", "lsm_p_area", "lsm_p_shape"))

saveRDS(gc_patch_landscape_scale_metrics, file= "data/rda_files/gc_patch_landscape_scale_metrics.Rda")

#### . ####
##### LM TESTING ####
# test if changing extents affect the landscape metrics we have chosen for the analysis
# create 2 landscape with 2 classes--each with multiple patches.
# one landscape needs to have a larger extent than the other, but both must have the same classes.
# make them spatraster objects. 

extent_small_df =  read_csv("data/test_landscapes/extent_small.csv", col_names = FALSE)
extent_small = as.matrix(extent_small_df)

extent_large_df = read_csv("data/test_landscapes/extent_large.csv", col_names = FALSE)
extent_large = as.matrix(extent_large_df)

# test 2 things: NAs as the background. Class = 0 as the background.
extent_small_rast <- rast(extent_small)
extent_large_rast <- rast(extent_large)

plot(extent_small_rast)
plot(extent_large_rast)

# test #1 -- keeping the background as class = 0.
check_landscape(extent_small_rast)
ext_small_lm <- calculate_lsm(extent_small_rast, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_c_lpi"))

a <- ext_small_lm %>% 
  filter(class != 0) %>% 
  View()

# test #2 -- keeping the background as class = 1.
check_landscape(extent_large_rast)
ext_large_lm <- calculate_lsm(extent_large_rast, what = c("lsm_c_ca", "lsm_c_np", "lsm_c_lsi", "lsm_c_ai", "lsm_c_lpi"))

ext_large_lm %>% 
  filter(class != 0) %>% 
  View()




