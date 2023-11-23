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
library(dichromat)
library(basemaps)
library(corrplot)
options(tigris_use_cache = TRUE)

rm(list=ls())

options(scipen=999)

########################################################################### NOTES #############################################################################

# The purpose of this file is for examining the water use of golf courses and relating the water use to golf course spatial pattern.

# NOTES FOR DATA ORGANIZATION
# KEY for TURF dataset (turf .xlsx file)
# red = abandoned golf course
# yellow = matched on polygons dataset (name changed)
# green = pinal county
# purple = indian reservation
# blue = on the turf/water datsets but not on polygon dataset

# key for water (water .xlsx file)
# red = abandoned golf course
# yellow = matched on polygons dataset (name changed)
# green = pinal county
# purple = indian reservation
# blue = on the turf/water datsets but not on polygon dataset
# orange = data is so bad that it is not being included

# MISSING FROM BOTH
# cottonwood country club (no idea why)
# rancho de los caballeros golf club (not in Phoenix AMA)
# paradise peak west
# wekopa (reservation?)
# talking stick (reservation?)
# wickenburg (not in AMA)
# whirlwind (reservation)

# MISSING FROM TURF ONLY
# silverleaf country club (no idea why)
# toka sticks (williams AFB) (no idea why)
# palo verde country club (no idea why)

# sun lakes country clubs
# https://www.sunlakesofarizona.com/cc_paloverde.php
# oakwood, ironwood, palo verde, cottonwood, sun lakes HOA

# sun city west
# https://suncitywest.businesshomepage.info/golf/ (probably listed as sun city west golf courses on the water data sheet)

##################################################################################################### DATA UPLOAD #######################################################################################################

# TODO insert the path to your working directory here:
# setwd("")

golf_course_scale_metrics <- readRDS(file= "data/rda_files/golf_course_scale_metrics.Rda")
golf_club_scale_metrics <- readRDS(file= "data/rda_files/golf_club_scale_metrics.Rda")

########################################################################### global cosmetic vars #########################################################################

gc_green = "#32CD32"

#### . ####
##################################################################################################### GC_POLYGONS #######################################################################################################

gc_polygons_sf <- readRDS("data/rda_files/gc_polygons_sf.Rda") 

#### . ####
##################################################################################################### WATER DATASET #######################################################################################################

# deal with granite falls issue
# granite falls is treated as the same course on polygons and turf datasets, but treated as two courses on water dataset. Combine them to be one on water dataset.
granite_falls_data <- read_csv("data/ADWR_public_record_requests/gc_water_use.csv") %>% 
  mutate(gc_name = str_replace_all(gc_name, " ", "_")) %>% 
  mutate(gc_name = str_replace_all(gc_name, ",", "_")) %>% 
  mutate(gc_name = tolower(gc_name)) %>% 
  filter(gc_name == "scg_granite_falls_north" | gc_name == "scg_-_granite_falls_south") %>%
  group_by(AMA, year, subsector) %>%
  summarise(total_use = sum(total_use), groundwater = sum(groundwater), CAP = sum(CAP), effluent = sum(effluent), surface_water = sum(surface_water), commingled = sum(commingled), other = sum(other), in_lieu = sum(in_lieu), spill = sum(spill)) %>%
  mutate(PCC = 25-225290.0000, .before = total_use) %>%  # matches the pcc for granite falls on the turf dataset
  mutate(gc_name = "scg_granite_falls", .before = total_use) %>% 
  mutate(ACTIVE = "Y")

# courses to exclude (see datasheet and key for water dataset; key located in notes) (many golf courses are not active/not in MC/ETC...)
# apache_sun [closed]
# dreamland_villa_gc [closed]
# el caro [closed]
# pepperwood gc [closed]
# cottonfields golf club [not on polygon dataset]
# riverview_gc is using small amount of water/probably obsolete
# sierra estrella (unsure)
# vistal_golf_club [closed]
# villa_monterey_gc [closed]
# falcon_gc [not on polygons dataset]
# rio_salado_gc [closed]
# villa_de_paz_gc [closed]
# adobe_dam_family_golf_center [closed]
# roles_inn_of_america [not on polygons]
# unsure what hale irwin golf course is--might be one of the palm valley courses.
# quick hits golf course [no trace of it anywhere]
# ahwatukee golf properties [no clue; no data anyway]

cniw <- c("apache_creek_golf_course", "asu_karsten_golf_course", "gold_canyon_golf_resort", "southern_ridge_golf_club", "roadhaven_golf_resort", "scottsdale_golf_center", "scottsdale_shadows_gc", "view_at_gold_ranch", "scg_desert_springs_granite_falls_north_and_south", "glen_lakes_municipal_gc", "toka_sticks_gc", "the_golf_club_at_johnson_ranch", "adobe_dam_golf_center", "precision_golf", "superstition_mtn_gc", "mountain_brook_gc", "golf_club_at_oasis", "encanterra_country_club", "victory_golf_course", "Sun_Corridor_datasetsun_city_west_golf_courses", "adwr_test_facility", "cortina_hoa", "victory_golf_course",
          "rcsc_lakes_east/west_dawn/viewpoint_lake", "sun_lakes_hoa_#3__ironwood_gc__oakwood_gc", "rcsc_south_&_quail_run", "scg_desert_springs__granite_falls_north_and_south__cimmaron", "apache_sun_golf_course", "dreamland_villa_gc", "el_caro_gc", "pepperwood_gc", "cottonfields_gc", "queen_valley_gc", "riverview_gc", "scr_-__willowbrook_/_willowcreek", "sierra_estrella_gc", "vistal_golf_club", "villa_monterey_gc", "falcon_gc", "view_at_gold_canyon_ranch", "links_at_queen_creek", "rio_salado_gc", "mesa_family_golf", "adobe_dam_family_golf_center", "villa_de_paz_gc", "roles_inn_of_america", "hale_irwin_golf_course",
          "pitch_&_putt", "ahwatukee_golf_properties", "quick_hits_golf_course")

# WATER DATASET MISSING DATA
# NOT VISIBLE in the .xlsx file from ADWR (BUT DOES EXIST)
# for whatever reason, tons of these courses are not visible on the .csv file. However, they are there. 
# see the .csv file that I wrote from the R dataframe. Those data include many of these missing courses.

# ahwatukee_counry_club 
# anthem golf club 
# arizona golf resort 
# augusta ranch gc 
# blackstone country club
# eagles nest golf club  
# foothills 
# great eagle 
# granite falls 
# cimarron  
# desert springs  (included but data does not make sense)
# las sendas
# moon valley
# orange tree resort
# palmbrook country club
# phoenix country club
# quail run
# scr riverview (sun city course)
# trilogy golf course at power ranch
# stonecreek
# sun city  country club
# sun city rec -- east
# sun city rec -- west
# sun city rec -- north
# sun city rec -- south
# sun city rec -- riverview
# sun city rec -- quail run
# sun city rec -- willowcreek
# sun city rec -- willowbrook
# sundance golf course
# arizona traditions
# union hills cc

# scw deer valley
# scw desert trails
# scw_echo_mesa 
# scw grandview 
# scw pebblebrook
# scw stardust
# scw trailridge

# DATA NOTES
# sun city lakes east includes "dawn lake" which is an HOA? may be best not to include it in the analysis [water dataset]
# sun city lakes west includes viewpoint lake [water dataset]
# encanto water use includes the encanto park
# verrado water use has high variance likely due to recent construction of another 18 hole course.
# willowbrook/willowcreek have data combined on the water dataset. maybe best to leave them out.
# palm valley golf course is 18 holes instead of 27 on the turf spreadsheet

# GOLF COURSES TO LEAVE OUT OF WATER DATASET
# willowbrook/willowcreek have data combined on the water dataset

# Cleaning annual water use data
# I am going to turn each row with a value of 0 to NA because it is not possible for an active golf course to not use any water for an entire year.
# Most of the instances of 0 in the datset are at the beginning or end of the temporal coverage. Lots of them are for the years 2021,2022,2023.
# however, some values of 0 are seemingly randomly distributed throughout the temporal coverage. I will turn those to NA.
# There are some really low values that are not 0 as well. I will keep those.

water_use_dataset <- read_csv("data/ADWR_public_record_requests/gc_water_use.csv") %>% 
  filter(AMA == "C") %>%
  mutate(gc_name = str_replace_all(gc_name, " ", "_")) %>% 
  mutate(gc_name = str_replace_all(gc_name, ",", "_")) %>% 
  mutate(gc_name = tolower(gc_name)) %>% 
  filter(!gc_name %in% cniw) %>% 
  filter(gc_name != "scg_-_granite_falls_south" & gc_name != "scg_granite_falls_north") %>% 
  rbind(granite_falls_data) %>% 
  mutate(gc_name = case_when(gc_name == "blackstone_@_vistancia" ~ "blackstone_country_club",
                             gc_name == "sun_city_rec_-_quail_run" ~ "scr_quail_run",
                             gc_name == "sun_city_rec_-_riverview" ~ "scr_riverview",
                             gc_name == "trilogy_golf_course" ~ "trilogy_golf_course_at_power_ranch",
                             gc_name == "scr_-_lakes_east_gc_&_dawn_lake" ~ "scr_east_gc",
                             gc_name == "scr_-_lakes_west_gc_&_viewpoint_lake" ~ "scr_west_gc",
                             gc_name == "sun_city_rec_-_north" ~ "scr_north_gc",
                             gc_name == "sun_city_rec_-_south" ~ "scr_south_gc",
                             gc_name == "sun_city_rec_-_riverview" ~ "scr_riverview",
                             gc_name == "sun_city_rec_-_quail_run" ~ "scr_quail_run",
                             gc_name == "scw_-_deer_valley_gc" ~ "scw_deer_valley",
                             gc_name == "scw_-_desert_trails" ~ "scw_desert_trails",
                             gc_name == "scw_-_echo_mesa" ~ "scw_echo_mesa",
                             gc_name == "scw_-_grandview" ~ "scw_grandview",
                             gc_name == "scw_-_pebblebrook_gc" ~ "scw_pebblebrook_gc",
                             gc_name == "scw_-_stardust" ~ "scw_stardust",
                             gc_name == "scw_-_trailridge" ~ "scw_trailridge",
                             gc_name == "scr_-__willowbrook_/_willowcreek" ~ "scr_willowbrook_willowcreek",
                             .default = gc_name)) %>% 
  filter(gc_name != "scr_willowbrook_willowcreek") %>% 
  filter(year != 2023) %>% # 2023 data is not complete
  mutate(total_use = ifelse(total_use == 0, NA, total_use)) # turn values of 0 to NA


str(water_use_dataset)

unique(water_use_dataset$gc_name)
setdiff(water_use_dataset$gc_name, gc_polygons_sf$gc_name) # none are in water use that are not in polygons
setdiff(gc_polygons_sf$gc_name, water_use_dataset$gc_name) 


#### @num holes analysis ####
# number of holes is a little difficult to determine for the water dataset because there is no column for number of holes on the water dataset. 
# there is some disagreement between the polygon dataset and the water dataset as to the number of holes/gc. There is no metadata for the turf dataset saying
# how up to date the number of holes is for each course. Many of the courses are expanding/building new holes. 
# I am going to assign the current number of holes (from polygon dataset) to the water dataset. 

# Mismatch between num holes in turf vs. polygon dataset:
# ahwatukee country club [turf: 36 holes, polygon: 18 holes]. It actually has 36 holes, so that should be the num_holes for the turf/water analysis.[FIXED]
# viewpoint golf and resort [turf: 27, polygon: 18]. Turf is correct. [FIXED]
# rio verde golf course [turf: 36, polygon: 18]. Turf is correct. [FIXED]
# ocotillo [turf: 27, polygon 18]. Turf is correct. Ocotillo has 3 9-hole courses. [FIXED]

# palm valley golf course  [turf: 18; polygon 27]. Turf is outdated.
# scr willow creek [turf: 36, polygon: 27]. Polygon is correct (see willowcreek website). 
# the golf club of scottsdale [turf: 18, polygon: 45]. Polygon is correct (see website).
# the phoenician [turf: 27, polygon: 18]. Polygon is correct (see Phoenician website)
# verrado golf course [turf: 18, polygon: 36]. Polygon is correct (see Verrado website)
# copper canyon golf club [turf: 18 holes, polygon: 27 holes]. Turf number is outdated.
# desert mountain [turf: 90 holes, polygon 108 holes]. Turf number is outdated. Desert mountain actually has 126 holes. Polygon dataset is missing course No.7. [FIXED]
# moon valley cc [turf: 36, polygon 18]. Polygon is correct [see moon valley website]


gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_name, course, num_holes) %>% 
  unique(.) %>% 
  group_by(gc_name) %>%
  summarise(num_holes = sum(num_holes)) %>%
  View()

#### @to join water use ####
# total water use
num_holes <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_name, course, num_holes) %>% 
  unique() %>% 
  group_by(gc_name) %>% 
  summarise(num_holes = sum(num_holes))

average_annual_water_use_df <-  water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc (must have at least 5 years of data)
  group_by(gc_name) %>% 
  summarise(average_annual_water_use = mean(total_use, na.rm = TRUE), num_years = n()) %>% 
  select(gc_name, average_annual_water_use) %>% 
  merge(., num_holes, by = "gc_name")

average_annual_water_use_per_hole_df <-  water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc (must have at least 5 years of data)
  group_by(gc_name) %>% 
  summarise(average_annual_water_use = mean(total_use, na.rm = TRUE), num_years = n()) %>% 
  merge(., num_holes, by = "gc_name") %>%
  mutate(aa_water_use_per_hole = average_annual_water_use/num_holes) %>% 
  select(gc_name, aa_water_use_per_hole) 


#### @variance in average annual water use/hole/golf course ####

# NOTE this is the number of holes from polygon cleaned. Once the data have been updated, this is the most up-to-date version of these data.
variance_fig <- water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc
  merge(., num_holes, by = "gc_name") %>%
  group_by(gc_name) %>% 
  mutate(annual_water_use_per_hole = total_use/num_holes) %>% 
  group_by(gc_name) %>%
  summarise(v = var(annual_water_use_per_hole, na.rm = TRUE), num_years = n()) %>%
  ggplot(aes(x = 1, y = v)) + 
  geom_violin(scale = "count", fill = "#63C5DA", col = "#63C5DA") + geom_boxplot(width=0.1) +
  scale_y_continuous(limits = c(0,1250), breaks = c(0,250,500,750,1000,1250), labels = c(0,250,500,750,1000,1250)) + ylab ("Variance in average annual \n water use/golf course/hole (AF)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 14),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 12),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

variance_fig

# Note: to be included, each golf course had to have at least 5 non-NA values anytime from 2000-2022. palo_verde_cc was the only course not meeting the criteria
# Note: there are some instances of golf course construction, which likely are inflating the variance by shifting golf course water use (substantial increase or decrease) (e.g., golf club of scottsdale)
# Note: there is lots of variation that is not explained by golf course construction (see annual water use for high variance courses)
# normalizing by num holes changes the results. 
# one idea here is to reduce the number of years to the last 5 years 2015-2020?

# INVESTIGATE OUTLIERS

# high variance courses
# estrella_golf_club: 18 holes [no evidence of construction]
# scr_east_gc: 18 holes [water use data may also include some HOA-related thing]
# trilogy_golf_club_at_vistancia: 18 holes [no evidence of construction; 2 outlying years]
# blackstone_country_club (at vistancia): 18 holes [no evidence of construction; 1 oultying year]
# scr_west_gc: 18 holes [water use data may also include some HOA-related thing]
# scr_quail_run: 18 holes [could be another HOA-related thing]
# sundance_golf_course: 18 holes [no evidence of construction]
# ahwatukee_country_club: 36 holes [this may change once num_holes gets updated.]

water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc
  merge(., num_holes, by = "gc_name") %>%
  group_by(gc_name) %>% 
  mutate(annual_water_use_per_hole = total_use/num_holes) %>% 
  group_by(gc_name) %>%
  summarise(v = var(annual_water_use_per_hole, na.rm = TRUE), num_years = n()) %>%
  View()

high_v <- c("estrella_golf_club", "scr_east_gc", "trilogy_golf_club_at_vistancia", "blackstone_country_club", "scr_west_gc", "scr_quail_run", "sundance_golf_course", "ahwatukee_country_club")

water_use_dataset %>% 
  filter(gc_name %in% high_v) %>%
  View()


#### @average annual water use/golf course/hole ####
average_annual_water_use_hole_fig <- water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc (must have at least 5 years of data)
  group_by(gc_name) %>% 
  summarise(average_annual_water_use = mean(total_use, na.rm = TRUE), num_years = n()) %>% 
  merge(., num_holes, by = "gc_name") %>%
  mutate(aa_water_use_per_hole = average_annual_water_use/num_holes) %>% 
  ggplot(aes(x = 1, y = aa_water_use_per_hole)) + 
  geom_violin(scale = "count", fill = "#63C5DA", col = "#63C5DA") +
  geom_boxplot(width=0.1) + scale_x_continuous(breaks = c(1), labels = "Frequency") + 
  scale_y_continuous(limits = c(0,80)) + ylab ("Water use/ \ngolf course/hole (AF)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

average_annual_water_use_hole_fig

#### @average annual water use/golf course ####
average_annual_water_use_fig <- water_use_dataset %>% 
  filter(gc_name != "palo_verde_cc") %>%  # getting rid of palo_verde_cc (must have at least 5 years of data)
  group_by(gc_name) %>% 
  summarise(average_annual_water_use = mean(total_use, na.rm = TRUE), num_years = n()) %>% 
  merge(., num_holes, by = "gc_name") %>%
  ggplot(aes(x = 1, y = average_annual_water_use)) + 
  geom_violin(scale = "count", fill = "#63C5DA", col = "#63C5DA") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(limits = c(0,1600)) + ylab ("Water use/ \ngolf course (AF)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 24),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 20),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

average_annual_water_use_fig


# INVESTIGATE OUTLIERS

#### @2020 water use/golf course/hole ####

water_use_2020_fig <- water_use_dataset %>% 
  filter(year == 2020) %>%
  merge(., num_holes, by = "gc_name") %>%
  mutate(aa_water_use_per_hole = total_use/num_holes) %>% 
  ggplot(aes(x = 1, y = aa_water_use_per_hole)) + 
  geom_violin(scale = "count", fill = "#63C5DA", col = "#63C5DA") +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.2, shape = 3, col = "#3944BC") +
  scale_y_continuous(limits = c(0,100)) + ylab ("Water use in 2020 \n per golf course \n per hole (AF)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 14),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 12),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

water_use_2020_fig

# NOTE: using 2020 will increase the chances that water use matches num_holes (because num_holes is based off current golf course composition)
# NOTE: couple NA values here because no data in 2020.



#### . ####
#### @total GC water use ####

# Note: visual examination of the data shows that the ADWR water dashboard data comes from the water_use_dataset. The annual variation in the water dashboard data matches our data very closely.
# The only difference between the ADWR water dashboard data and out data is that ours is for Maricopa County and theirs is for the Phoenix AMA.


#### ...ADWR data request ####

gc_water_use_MC <- water_use_dataset %>%
  filter(year != 2021 & year != 2022) %>% # seems like there is missing data for 2021, 2022 
  group_by(year) %>% 
  summarise(total_use = sum(total_use, na.rm = TRUE), groundwater = sum(groundwater, na.rm = TRUE), CAP = sum(CAP, na.rm = TRUE), effluent = sum(effluent, na.rm = TRUE), surface_water = sum(surface_water, na.rm = TRUE), commingled = sum(commingled, na.rm = TRUE), other = sum(other, na.rm = TRUE)) %>% 
  gather(., sector, quantity, groundwater:other) %>% 
  mutate(sector = factor(sector, levels = c("groundwater", "CAP", "surface_water", "effluent", "commingled", "other"))) %>%
  # mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = year, y = quantity, fill = sector)) + geom_bar(position = 'stack', stat = 'identity', width = 0.80) + xlab("Year") + ylab("Golf course \n water use \n (Thousand AF)") +
  scale_y_continuous(limits = c(0,100000), breaks = c(0,25000,50000,75000,100000), labels = c("0", "25", "50", "75", "100")) + 
  scale_fill_discrete(name = "Sector", labels = c("groundwater", "CAP", "surface water", "effluent", "commingled", "other")) + 
  scale_x_continuous(breaks = c(2000,2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020), labels = c("2000", "", "2002", "", "2004", "", "2006", "", "2008", "", "2010", "", "2012", "", "2014", "", "2016", "", "2018", "", "2020")) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 15),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 13),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 30), size = 15),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 13),
    legend.text=element_text(size=13),
    legend.title=element_text(size=13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

gc_water_use_MC

#### ...ADWR water dashboard ####

# Load data
# a.k.a. dissertation chap 2/landscape drivers data/water/AMA_Demand_Supply_from_DW.csv
AMA_water_path = 'data/ADWR_publicly_available_data/ADWR_supply_demand_dashboard.csv'
AMA_water <- read_csv(AMA_water_path)

# second sheet of the "industrial" source data from ADWR water dashboard
turf_water_use_path2 = 'data/ADWR_publicly_available_data/ADWR_industrial_dashboard.csv'
turf_water2 <- read_csv(turf_water_use_path2)

# total golf course water use; this matches the ADWR values on the ADWR water dashboard
gc_water_use <- turf_water2 %>% 
  filter(AMA == 'Phoenix') %>% 
  filter(Subsector == "FACILITY TURF - GOLF COURSE") %>%
  dplyr::select(-ACTIVE) %>% # To get the values on the ADWR dashboard, you need to remove duplicated rows. the ACTIVE column prevents all distinct rows from being removed because it has 'y' and 'n' values in rows where everything else is the same
  distinct() %>% # gets rid of duplicated rows
  dplyr::select(AMA, year, ALLOT_QTY, total_use) %>%
  mutate(year = factor(year)) %>%
  group_by(year) %>%
  summarise(sum_alloted = sum(ALLOT_QTY, na.rm = TRUE), sum_use = sum(total_use)) %>%
  dplyr::select(sum_use, year) %>% 
  rename("quantity" = sum_use) %>% 
  mutate(sector = "Industrial_GC")

pal <- c( "#00BFC4", "#00BA38")

hex <- hue_pal()(6)
show_col(hex)

gc_water_use_AMA <- AMA_water %>% 
  filter(AMA == "PHOENIX AMA") %>%
  filter(category == "Supply") %>%
  group_by(sector, year) %>% 
  summarise(quantity = sum(quantity)) %>% 
  mutate(sector = factor(sector, levels = c("Agricultural", "Indian", "Industrial", "Municipal"))) %>%
  ungroup() %>% 
  filter(year > 1999) %>% 
  rbind(gc_water_use) %>% 
  spread(., sector, quantity) %>% 
  mutate(industrial_other = Industrial - Industrial_GC) %>%
  mutate(other_water = industrial_other + Agricultural + Municipal + Indian) %>%
  dplyr::select(other_water, Industrial_GC, year) %>%
  mutate(total = other_water + Industrial_GC) %>% 
  mutate(gc_percent = Industrial_GC/total) %>% 
  mutate(other_precent = other_water/total) %>% 
  gather(., sector, percent, gc_percent:other_precent) %>%
  select(-other_water, -Industrial_GC, -total) %>% 
  mutate(sector = factor(sector, levels = c("other_precent", "gc_percent"))) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = percent, fill = sector)) + geom_bar(position = 'stack', stat = 'identity', width = 0.80) + 
  scale_fill_manual(values = pal, name = "Water Use", labels = c("all uses", "golf courses")) + xlab("Year") + ylab("Total water \n use (%)") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0", "25", "50", "75", "100")) + scale_x_continuous(breaks = c(2000,2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020), labels = c("2000", "", "2002", "", "2004", "", "2006", "", "2008", "", "2010", "", "2012", "", "2014", "", "2016", "", "2018", "", "2020")) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 30), size = 15),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 13),
    legend.text=element_text(size=13),
    legend.title=element_text(size=13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # legend.position = "bottom",
    axis.line = element_line(colour = "black"))

gc_water_use_AMA

industry_pal <- c("#F564E3", "#00BA38")

industry_fig <- AMA_water %>% 
  filter(AMA == "PHOENIX AMA") %>%
  filter(category == "Supply") %>%
  group_by(sector, year) %>% 
  summarise(quantity = sum(quantity)) %>% 
  mutate(sector = factor(sector, levels = c("Agricultural", "Indian", "Industrial", "Municipal"))) %>%
  ungroup() %>% 
  filter(year > 1999) %>% 
  rbind(gc_water_use) %>% 
  spread(., sector, quantity) %>% 
  mutate(industrial_other = Industrial - Industrial_GC) %>%
  dplyr::select(industrial_other, Industrial_GC, year) %>%
  mutate(total = industrial_other + Industrial_GC) %>%
  mutate(gc_percent = Industrial_GC/total) %>%
  mutate(industrial_other_percent = industrial_other/total) %>%
  gather(., sector, percent, gc_percent:industrial_other_percent) %>%
  select(year, sector, percent) %>%
  mutate(sector = factor(sector, levels = c("industrial_other_percent", "gc_percent"))) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = percent, fill = sector)) + geom_bar(position = 'stack', stat = 'identity', width = 0.80) + 
  scale_fill_manual(values = industry_pal, name = "Water Use", labels = c("industrial - other", "industrial - golf course")) + xlab("Year") + ylab("Total water \n use (%)") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0", "25", "50", "75", "100"))+ scale_x_continuous(breaks = c(2000,2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020), labels = c("2000", "", "2002", "", "2004", "", "2006", "", "2008", "", "2010", "", "2012", "", "2014", "", "2016", "", "2018", "", "2020")) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 30), size = 15),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 13),
    legend.text=element_text(size=13),
    legend.title=element_text(size=13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # legend.position = "bottom",
    axis.line = element_line(colour = "black"))

industry_fig

#### @combined fig####
(industry_fig) + (gc_water_use_AMA) + (gc_water_use_MC) + plot_layout(ncol = 1, nrow = 3) + plot_annotation(tag_levels = "a") & theme(
  plot.tag = element_text(size = 18),
  plot.subtitle = element_text(size = 10))

ggsave(filename = "photos/final_figures/gc_water_use.png", width = 6, height = 3, scale = 1.8)

# Graphic %
# total water use time series stacked by water source.
# Sectoral water use across AMA?

#### . ####
##################################################################################################### TURF DATASET #######################################################################################################

# DATA NOTES
# the golf club of scottsdale is only 18 holes on the turf dataset. It is more than 18 holes on the polygon and the water dataset. Maybe it should be excluded from water and turf analysis due to the mismatch..

# GOLF COURSES TO LEAVE OUT OF TURF DATASET
# verrado golf course
# golf club of scottsdale
# palm valley?

# these courses not included are either not active, not on the polygon dataset, or not in Maricopa County. See gc_acreage.xlsx and key above.
cnit <- c("queen_valley_gc", "glen_lakes_municipal_gc", "villa_de_paz_gc", "scottsdale_shadows_gc", "roles_inn_of_america", "roadhaven_golf_resort", "apache_sun_golf_course", "roadhaven_golf_resort",
          "apache_sun_golf_course", "gold_canyon_golf_resort", "view_at_gold_canyon_ranch", "adobe_dam_family_golf_center", "apache_creek_golf_course", "links_at_queen_creek", "mountain_brook_gc",
          "the_golf_club_at_johnson_ranch", "falcon_gc", "superstition_mtn_gc", "golf_club_at_oasis", "encanterra_country_club")

turf <- read_csv("data/ADWR_public_record_requests/gc_acreage.csv") %>% 
  mutate(gc_name = str_replace_all(gc_name, " ", "_")) %>% 
  mutate(gc_name = str_replace_all(gc_name, ",", "_")) %>% 
  mutate(gc_name = tolower(gc_name)) %>% 
  filter(!gc_name %in% cnit) %>% 
  filter(gc_name != "verrado_golf_club" & gc_name != "the_golf_club_of_scottsdale") %>% 
  mutate(total_turfed_ha = total_turfed_acres*0.4047) %>% 
  mutate(total_water_surface_ha = total_water_surface_area*0.4047)

setdiff(turf$gc_name, gc_polygons_sf$gc_name) # none are in acreage that are not in polygons.
setdiff(gc_polygons_sf$gc_name, turf$gc_name) # the expected ones are in polygons and not in acreage.

# scatterplot of turfed acres/hole
total_turfed_ha <- turf %>% 
  mutate(turfed_ha_per_hole = total_turfed_ha/num_holes) %>% 
  mutate(ifelse(year_built < 1985, "pre_1985", "post_1985")) %>% 
  ggplot(aes(x = 1, y = turfed_ha_per_hole)) + 
  geom_violin(scale = "count", fill = "#32CD32", col = "#32CD32") + 
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.2, shape = 3, col = "#000000") +
  scale_y_continuous(limits = c(0,5)) + ylab ("Total turfed acres/golf course/hole") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_turfed_ha

# Note: num_holes is the value listed in the turf dataset. Even though that value is outdated for some courses, it must be used to match the attribute data in turf dataset.

#### @turf area/gc/hole  ####
total_turfed_ha_hole_GMA <- turf %>% 
  mutate(turfed_ha_per_hole = total_turfed_ha/num_holes) %>% 
  mutate(year_cat = ifelse(year_built < 1985, "pre_1985", "post_1985")) %>%
  select(turfed_ha_per_hole, year_cat) %>% 
  mutate(year_cat = factor(year_cat, levels = c("pre_1985", "post_1985"))) %>% 
  ggplot(aes(x = year_cat, y = turfed_ha_per_hole)) + geom_violin(scale = "count", fill = gc_green, col = gc_green) +
  geom_boxplot(width=0.1) + scale_x_discrete(labels = c("pre_1985" = "Pre GMA", "post_1985" = "Post GMA")) + scale_y_continuous(limits = c(0,5), breaks = c(0,1,2,3,4,5)) +
  ylab ("Turf/golf course/ \n hole (ha)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_turfed_ha_hole_GMA

#### @turf area/gc  ####
total_turfed_ha_GMA <- turf %>% 
  mutate(year_cat = ifelse(year_built < 1985, "pre_1985", "post_1985")) %>%
  select(total_turfed_ha, year_cat) %>% 
  mutate(year_cat = factor(year_cat, levels = c("pre_1985", "post_1985"))) %>% 
  ggplot(aes(x = year_cat, y = total_turfed_ha)) + geom_violin(scale = "count", fill = gc_green, col = gc_green) +
  geom_boxplot(width=0.1) + scale_x_discrete(labels = c("pre_1985" = "Pre GMA", "post_1985" = "Post GMA")) + scale_y_continuous(limits = c(0,200), breaks = c(0,2,4,6,8,10,12)) +
  ylab ("Turf/golf course/ (acres)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_turfed_ha_GMA

#### @water area  ####
# total water area per golf course
water_area_GMA <- turf %>% 
  mutate(year_cat = ifelse(year_built < 1985, "pre_1985", "post_1985")) %>%
  select(total_water_surface_ha, year_cat) %>%
  mutate(year_cat = factor(year_cat, levels = c("pre_1985", "post_1985"))) %>%
  ggplot(aes(x = year_cat, y = total_water_surface_ha)) + geom_violin(scale = "count", fill = gc_green, col = gc_green) +
  geom_boxplot(width=0.1) + scale_x_discrete(labels = c("pre_1985" = "Pre GMA", "post_1985" = "Post GMA")) + scale_y_continuous(limits = c(0,40), breaks = c(0,10,20,30,40)) +
  ylab ("Water surface area/ \n golf course (ha)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

water_area_GMA

# top surface water areas:
# Mccormick Ranch: tons of water

# NOTE: some of the water that is counted for the golf course is not actually part of the golf course per se, but instead part of the homeowners association (see sc_west/sc_east)

#### @average annual water use/gc/hole ####
water_use_hole_GMA <-  turf %>% 
  select(gc_name, year_built) %>% 
  merge(., average_annual_water_use_per_hole_df, by = "gc_name") %>% 
  mutate(year_cat = ifelse(year_built < 1985, "pre_1985", "post_1985")) %>%
  select(aa_water_use_per_hole, year_cat) %>%
  mutate(year_cat = factor(year_cat, levels = c("pre_1985", "post_1985"))) %>%
  ggplot(aes(x = year_cat, y = aa_water_use_per_hole)) + geom_violin(scale = "count", fill = "#63C5DA", col = "#63C5DA") +
  geom_boxplot(width=0.1) + scale_x_discrete(labels = c("pre_1985" = "Pre GMA", "post_1985" = "Post GMA")) + scale_y_continuous(limits = c(0,80), breaks = c(0,20,40,60,80)) +
  ylab ("Water use/ \n golf course/hole (AF)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

water_use_hole_GMA

#### @average annual water use/gc ####

water_use_GMA <-  turf %>% 
  select(gc_name, year_built) %>% 
  merge(., average_annual_water_use_df, by = "gc_name") %>% 
  mutate(year_cat = ifelse(year_built < 1985, "pre_1985", "post_1985")) %>%
  select(average_annual_water_use, year_cat) %>%
  mutate(year_cat = factor(year_cat, levels = c("pre_1985", "post_1985"))) %>%
  ggplot(aes(x = year_cat, y = average_annual_water_use)) + geom_violin(scale = "count", fill = gc_green, col = gc_green) +
  geom_boxplot(width=0.1) + scale_x_discrete(labels = c("pre_1985" = "Pre GMA", "post_1985" = "Post GMA")) + scale_y_continuous(limits = c(0,1600), breaks = c(0,20,40,60,80)) +
  ylab ("Water use/ \n golf course (AF)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 20), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

water_use_GMA

#### @combined fig ####
(average_annual_water_use_hole_fig) + (water_use_hole_GMA) + (total_turfed_ha_hole_GMA) + (water_area_GMA)  + plot_layout(ncol = 2, nrow = 2) + plot_annotation(tag_levels = "a") & theme(
  plot.tag = element_text(size = 20),
  plot.subtitle = element_text(size = 10))

ggsave(filename = "photos/final_figures/GMA_effect.png", width = 8, height = 6, scale = 1.2)


#### @temporal golf course analysis ####

# when were the golf courses constructed
turf %>% 
  count(year_built) %>% 
  # summarise(annual_sum = sum()) %>% 
  ggplot(aes(x = year_built, y = n)) + geom_bar(stat = "identity")


#### . ####
##################################################################################################### Relationships between GC attributes #######################################################################################################

# NOTES
# There is a mismatch between the water use dataset and the LM dataset.
# The water use dataset does not divide the water use by 18/9 hole units for courses with more than 18 holes. It reports a value for the gc_name.
# The LM dataset calculated LM by 18/9 hole units.

# There are two potential ways to deal with this:
# 1.) calculate LSM according to the way the water dataset deals with courses. In other words, classify each course by 'gc_name' not 'gc_id'. For example, aguila golf course would be reclassified into one class with 27 holes. 
# This option would not hold total area constant at all, which would completely mask the effect of other landscape metrics on water use and also create many extreme course area values. However, it would most accurately represent
# the comparisons we are trying to do.

# 2.) calculate water use/hole for each gc_name and use that for the comparisons. This means that the same water use/hole value would be used for all course names belonging to each golf course. For example, all desert mountain golf courses would have the same water use/hole.
# this option would hold total golf course area more constant. We could even filter out the 9-hole courses to test relationships among only 18-hole courses.
# the problem with this option is that the same water/hole on every course name is not accurate. Surely the desert mountain golf courses with different areas are using different amounts of water.

# 3.) Summarising landscape metrics by gc_name (e.g., mean, sum) only works for "ca", so that is not an option.

# SOLUTION: Option 1 (see gc_name_level_metrics.rds)
# In all cases, composition is not held constant. Thus, composition will likely mask the effects of configuration on water use.

# Option 1
# Couple ways to handle option 1. The problem is the turf dataset has 8 observations with outdated number of holes and turf area. Thus, those 8 observations will not be good for comparing to the other variables.
# option 1a is to just get rid of those 8 observations in the turf dataset and investigate all relationships (N = 137).
# option 1b is to keep those 8 observations and calculate turf/golf course/hole and use that to investigate the rest of the relationships.
# option 1b means that water use would have to be water use/golf course/hole.
# option 1b is problematic because all landscape metrics have been calculated by 'gc_name_id' and not 'gc_id'.

# SOLUTION
# option 1a seems to be the simplest and it does not change the relationships among landscape metrics and water use/golf course. (see correlation matrix analysis)
# option 1a also keeps everything on the golf course level.

#### . ####
#### data ####
gc_names_to_join <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_name_id, gc_name) %>% 
  unique()

lpa_to_join <- golf_club_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_name_id" = "class") %>%
  select(gc_name_id, value) %>%
  group_by(gc_name_id) %>%
  summarise(lpa = max(value))

# FULL DF
# this dataframe does not have the missing courses that have incorrect turf data. Turf data is not joined.
joined_full_df <- golf_club_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_name_id" = "class") %>%
  merge(., lpa_to_join, by = "gc_name_id") %>% 
  merge(., gc_names_to_join, by = "gc_name_id") %>% 
  merge(., average_annual_water_use_df, by = "gc_name") %>% 
  mutate(log_ca = log(ca), log_lsi = log(lsi), log_ai = log(ai), log_lpa = log(lpa), log_water_use = log(average_annual_water_use)) 

correlation_full_df <- joined_full_df %>% 
  select(log_ca, log_lsi, log_ai, log_lpa, log_water_use)

# number of observations of full df: 147

# PARTIAL DF (all vars together, but less observations)
# List of courses with the incorrect number of holes for the turf dataset:
# palm valley golf course  [turf: 18; polygon 27]. Turf is outdated.
# scr willow creek [turf: 36, polygon: 27]. Polygon is correct (see willowcreek website). 
# the golf club of scottsdale [turf: 18, polygon: 45]. Polygon is correct (see website).
# the phoenician [turf: 27, polygon: 18]. Polygon is correct (see Phoenician website)
# verrado golf course [turf: 18, polygon: 36]. Polygon is correct (see Verrado website)
# copper canyon golf club [turf: 18 holes, polygon: 27 holes]. Turf number is outdated.
# desert mountain [turf: 90 holes, polygon 108 holes]. Turf number is outdated. Desert mountain actually has 126 holes. Polygon dataset is missing course No.7. [FIXED]
# moon valley cc [turf: 36, polygon 18]. Polygon is correct [see moon valley website]

turf_to_join <- turf %>%
  filter(gc_name != "palm_valley_golf_course" & gc_name != "scr_willowcreek" & gc_name != "the_golf_club_of_scottsdale" & gc_name != "the_phoenician_resort" & gc_name != "verrado_golf_club" & gc_name != "copper_canyon_golf_club" & gc_name != "desert_mountain_golf_course" & gc_name != "moon_valley_cc") %>% 
  select(year_built, total_turfed_ha, total_water_surface_area, total_water_surface_ha, gc_name) 

# this dataframe has 8 missing courses that do not have reliable turf data. 
joined_partial_df <- golf_club_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_name_id" = "class") %>%
  merge(., lpa_to_join, by = "gc_name_id") %>% 
  merge(., gc_names_to_join, by = "gc_name_id") %>% 
  merge(., average_annual_water_use_df, by = "gc_name") %>%
  merge(., turf_to_join, by = "gc_name") %>% 
  mutate(log_ca = log(ca), log_lsi = log(lsi), log_ai = log(ai), log_lpa = log(lpa), log_water_use = log(average_annual_water_use)) %>% 
  mutate(total_water_surface_area = total_water_surface_area + 0.01) %>%  # makes the log transformation work
  mutate(total_water_surface_ha = total_water_surface_ha + 0.01) %>%  # makes the log transformation work
  mutate(log_turf_area = log(total_turfed_ha), log_water_area = log(total_water_surface_ha))

correlation_partial_df <- joined_partial_df %>% 
  select(log_turf_area, log_water_area, log_ca, log_lsi, log_ai, log_lpa, log_water_use) %>% 
  rename("log(Turf area)" = "log_turf_area") %>% 
  rename("log(Water area)" = "log_water_area") %>% 
  rename("log(CA)" = "log_ca") %>% 
  rename("log(LSI)" = "log_lsi") %>% 
  rename("log(AI)" = "log_ai") %>% 
  rename("log(LPA)" = "log_lpa") %>% 
  rename("log(Water use)" = "log_water_use") 

#### . ####
#### EDA ####


#### @turf ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(total_turfed_ha)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_turf_area)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### @water surface area ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(total_water_surface_ha)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  mutate(log_total_water_surface_ha = log(total_water_surface_area)) %>% 
  ggplot(aes(log_turf_area)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### @water use ####
joined_partial_df %>% 
  ggplot(aes(average_annual_water_use)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>%  # little better with the log.
  ggplot(aes(log_water_use)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### @ca ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(ca)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_ca)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### @lsi ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(lsi)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_lsi)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### @ai ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(ai)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_ai)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))


#### @lpa ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(lpa)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_lpa)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))


#### @water_use ####
joined_partial_df %>% # distribution is not normal
  ggplot(aes(average_annual_water_use)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

joined_partial_df %>% # distribution is now very normal.
  ggplot(aes(log_water_use)) + geom_histogram(bins = 30) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))

#### . ####
#### scatterplots ####
####  @water use ~ total turfed acres ####
turf_fig <- joined_partial_df %>% 
  ggplot(aes( x = total_turfed_ha, y = average_annual_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + xlab("Turf area") + 
  ylab("Water use") + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 10), size = 14),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 12),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

turf_fig

log_turf_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_turf_area, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + xlab("log(Turf area)") + ylab("log(Water use)") + 
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 10), size = 14),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 12),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

log_turf_fig

####  @water use ~ total water surface area ####
water_surface_fig <- joined_partial_df %>% 
  ggplot(aes( x = total_water_surface_area, y = average_annual_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + scale_y_continuous() +
  scale_x_continuous() + xlab("Water surface area") + ylab("Water use") + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

water_surface_fig

log_water_surface_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_water_area, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) +
  scale_x_continuous(limits =c(-5,5), breaks = c(-5,-2.5,0,2.5,5)) + xlab("log(Water area)") + ylab("Average annual water use \n log(AF)") +
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

log_water_surface_fig

#### @water use ~ ca ####

CA_water_use_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_ca, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + scale_y_continuous() +
  scale_x_continuous(limits = c(2,6), breaks = c(2,3,4,5,6)) + xlab("log(CA)") + ylab("Average annual water use \n log(AF)") + 
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

CA_water_use_fig

#### @water use ~ lsi ####

LSI_water_use_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_lsi, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + scale_y_continuous() +
  scale_x_continuous(limits = c(0,2.5), breaks = c(0,0.5,1,1.5,2,2.5)) + xlab("log(LSI)") + ylab("log(Water use)") +
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

LSI_water_use_fig

#### @water use ~ ai ####

AI_water_use_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_ai, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + scale_y_continuous() +
  scale_x_continuous() + xlab("log(AI)") + ylab("log(Water use)") + 
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

AI_water_use_fig

#### @water use ~ lpa ####

LPA_water_use_fig <- joined_partial_df %>% 
  ggplot(aes( x = log_lpa, y = log_water_use)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, se = FALSE) + scale_y_continuous() +
  scale_x_continuous(limits = c(2.5, 5.5), breaks = c(2.5,3,3.5,4,4.5,5,5.5)) + xlab("log(LPA)") + ylab("log(Water use)") +
  scale_y_continuous(limits = c(4,8), breaks = c(4,5,6,7,8)) + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 20, l = 10), size = 14),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 10), size = 14),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 12),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

LPA_water_use_fig

#### combined fig ####
(log_turf_fig) + (log_water_surface_fig) + (CA_water_use_fig) + (LPA_water_use_fig) + (LSI_water_use_fig) + (AI_water_use_fig)  + plot_layout(ncol = 3, nrow = 2)

ggsave(filename = "photos/final_figures/scatterplots.png", width = 9, height = 6, scale = 1.0)

#### . ####
#### @correlation matrix ####

# # Version A
# png(filename="/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/final_figures/corrplot_Phoenix_squared.png", width = 1000, height = 1000,
#     units = "px", pointsize = 16)
# detrended_matrix_Phoenix <- cor(correlation_dataset_Phoenix) # pearson is the default 
# detrended_matrix_Phoenix_squared <- apply(detrended_matrix_Phoenix, c(1,2), function(x) (x^2) * (sqrt(x^2)/x)) # this contains the r-squared values
# detrended_test_matrix_Phoenix <- cor.mtest(correlation_dataset_Phoenix, conf.level = 0.95)
# corrplot(detrended_matrix_Phoenix_squared, method = 'color', p.mat = detrended_test_matrix_Phoenix$p, insig = 'blank', number.cex = 1.2, tl.cex = 1.2, type = 'lower')$corrPos -> p1
# text(p1$x, p1$y, round(p1$corr,2))
# dev.off()



#### @P-values ####
png(filename="photos/final_figures/corrplot_significance_levels.png", width = 1000, height = 1000,
    units = "px", pointsize = 16)
mat <- cor(correlation_partial_df) # pearson is the default 
values_mat <- cor.mtest(correlation_partial_df, conf.level = 0.95)
corrplot(mat, method = 'color', p.mat = values_mat$p, type = 'lower', sig.level = c(0.001, 0.01, 0.05), pch.cex = 2.4,
         insig = 'label_sig', number.cex = 3.0, tl.cex = 2.0)$corrPos -> p1
dev.off()

#### @R values ####
png(filename="photos/final_figures/corrplot_r_values.png", width = 1000, height = 1000,
    units = "px", pointsize = 16)
mat <- cor(correlation_partial_df) # pearson is the default 
values_mat <- cor.mtest(correlation_partial_df, conf.level = 0.95)
corrplot(mat, method = 'color', addCoef.col = 'black', type = 'lower', number.cex = 2.0, tl.cex = 2.0)$corrPos -> p1
dev.off()

#### @R^2 values ####
png(filename="photos/final_figures/corrplot_r2_values.png", width = 1000, height = 1000,
    units = "px", pointsize = 16)
mat <- cor(correlation_partial_df) # pearson is the default 
values_matrix <- cor.mtest(correlation_partial_df, conf.level = 0.95)
corrplot(mat*mat, method = "color", addCoef.col = 'black', type = 'lower', number.cex = 2.0, tl.cex = 2.0)$corrPos -> p1
dev.off()

values_matrix$p

# TODO
# change all units to km^2

#### . ####
#### reported values ####
# private vs. semi-private vs. resort vs. public
gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, status) %>% 
  unique() %>% 
  group_by(status) %>%
  summarise(count = n()) %>%
  View()

# golf courses use 3-4 percent of total water 
AMA_water %>% 
  filter(AMA == "PHOENIX AMA") %>%
  filter(category == "Supply") %>%
  group_by(sector, year) %>% 
  summarise(quantity = sum(quantity)) %>% 
  mutate(sector = factor(sector, levels = c("Agricultural", "Indian", "Industrial", "Municipal"))) %>%
  ungroup() %>% 
  filter(year > 1999) %>% 
  rbind(gc_water_use) %>% 
  spread(., sector, quantity) %>% 
  mutate(industrial_other = Industrial - Industrial_GC) %>%
  mutate(other_water = industrial_other + Agricultural + Municipal + Indian) %>%
  dplyr::select(other_water, Industrial_GC, year) %>%
  mutate(total = other_water + Industrial_GC) %>% 
  mutate(gc_percent = Industrial_GC/total) %>% 
  mutate(other_precent = other_water/total) %>% 
  gather(., sector, percent, gc_percent:other_precent) %>%
  select(-other_water, -Industrial_GC, -total) %>% 
  mutate(sector = factor(sector, levels = c("other_precent", "gc_percent"))) %>%
  mutate(year = as.numeric(year)) %>%
  View()

# golf courses use 50% of industrial water use
AMA_water %>% 
  filter(AMA == "PHOENIX AMA") %>%
  filter(category == "Supply") %>%
  group_by(sector, year) %>% 
  summarise(quantity = sum(quantity)) %>% 
  mutate(sector = factor(sector, levels = c("Agricultural", "Indian", "Industrial", "Municipal"))) %>%
  ungroup() %>% 
  filter(year > 1999) %>% 
  rbind(gc_water_use) %>% 
  spread(., sector, quantity) %>% 
  mutate(industrial_other = Industrial - Industrial_GC) %>%
  dplyr::select(industrial_other, Industrial_GC, year) %>%
  mutate(total = industrial_other + Industrial_GC) %>%
  mutate(gc_percent = Industrial_GC/total) %>%
  mutate(industrial_other_percent = industrial_other/total) %>%
  gather(., sector, percent, gc_percent:industrial_other_percent) %>%
  select(year, sector, percent) %>%
  View()

# water sources
water_use_dataset %>%
  filter(year != 2021 & year != 2022) %>% # seems like there is missing data for 2021, 2022 
  group_by(year) %>% 
  summarise(total_use = sum(total_use, na.rm = TRUE), groundwater = sum(groundwater, na.rm = TRUE), CAP = sum(CAP, na.rm = TRUE), effluent = sum(effluent, na.rm = TRUE), surface_water = sum(surface_water, na.rm = TRUE), commingled = sum(commingled, na.rm = TRUE), other = sum(other, na.rm = TRUE)) %>% 
  gather(., sector, quantity, groundwater:other) %>% 
  mutate(sector = factor(sector, levels = c("groundwater", "CAP", "surface_water", "effluent", "commingled", "other"))) %>%
  group_by(sector) %>% 
  summarise(mean = mean(quantity)) %>% 
  View()

# temporal golf course distribution
turf %>% 
  count(year_built) %>% 
  summarise(sum = sum(n)) %>% 
  View()

turf %>% 
  count(year_built) %>% 
  filter(year_built > 1969) %>% 
  summarise(sum = sum(n)) %>% 
  View()

# percentage of golf courses being built between 1970-2007 (119/146) =  81.5%


#### . ####
#### @test datasets ####
# to test the results when all observations are included for the water data/lm data (see notes at the beginning of section)
# the parameters are a little different than the saved version below.
mat <- cor(correlation_full_df) # pearson is the default 
values_mat <- cor.mtest(correlation_full_df, conf.level = 0.95)
corrplot(mat, method = 'color', p.mat = values_mat$p, type = 'lower', sig.level = c(0.001, 0.01, 0.05), pch.cex = 2.4,
         insig = 'label_sig', number.cex = 3.0, tl.cex = 1.6)$corrPos -> p1

# to test the results when some observations have not been included 
mat <- cor(correlation_partial_df) # pearson is the default 
values_mat <- cor.mtest(correlation_partial_df, conf.level = 0.95)
corrplot(mat, method = 'color', p.mat = values_mat$p, type = 'lower', sig.level = c(0.001, 0.01, 0.05), pch.cex = 2.4,
         insig = 'label_sig', number.cex = 3.0, tl.cex = 1.6)$corrPos -> p1
#  the relationships are the same with the 8 observations and without them.
