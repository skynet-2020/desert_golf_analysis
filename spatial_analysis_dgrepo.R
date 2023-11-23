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
options(tigris_use_cache = TRUE)


rm(list=ls())

options(scipen=999)

########################################################################### NOTES #############################################################################

# NOTE: THINGS TO LOOK AT WITH NEW DATA
# variation in water use per gc.
# total water use?
# water use/AF of turf?
# water use/18 holes?
# variation in average annual water use
# average annual water use ~ turf area
# variation in average annual water use ~ turn area.
# create a score for golf courses -- that incorporates surface water area, water use, [greenness?], see if it is correlated with MHHI.

# nice to compare turf area with polygon area.

########################################################################### LOAD LUCC DATA ########################################################################## 

# TODO insert the path to your working directory here:
# setwd("")

gc_polygons_sf <- readRDS("data/rda_files/gc_polygons_sf.Rda") 

gc_polygons <- gc_polygons_sf %>% 
  vect()

# MAG data
MC_LUCC <- st_read("data/LUCC/Existing_Land_Use__2022.shp") %>%
  filter(COUNTY == "Maricopa County") %>%
  filter(LONG_DISPL != "Golf Course") %>%
  rbind(select(gc_polygons_sf, -gc_name, -course, -gc_id, -gc_name_id, -unique_patch_id, -num_holes, -maintenanc, -status)) %>%
  vect()

gc_pts_sf <- gc_polygons_sf %>% 
  st_centroid() %>% 
  mutate(x_coord = unlist(map(geometry,1)),y_coord = unlist(map(geometry,2))) %>% # extract the coordinates
  mutate(gc_id = factor(gc_id)) %>%
  group_by(gc_id) %>%
  st_drop_geometry() %>% # get rid of the geometry so "summarise()" works
  summarise(across(c(x_coord, y_coord), mean)) %>%
  st_as_sf(., coords = c("x_coord", "y_coord"), crs = terra::crs(gc_polygons)) 

gc_pts <- gc_pts_sf%>%
  vect()

plot(gc_pts)

########################################################################### LOAD ADMINISTRATIVE DATA ########################################################################## 

# Phx freeways
freeways <- st_read("data/boundary_data/freeways/Freeways.shp") %>%
  sf::st_transform(terra::crs(gc_polygons))%>%
  vect()

# Phoenix AMA Boundary
ama_file <- terra::vect("data/boundary_data/PhxAMA/AMA_and_INA.shp") # takes the AMA file from chapter 2
phx_ama <- st_as_sf(ama_file)%>%
  filter(BASIN_NAME == "PHOENIX AMA") %>%
  st_transform(terra::crs(gc_polygons)) %>% 
  vect()

# lower 48 states
cont_us <- tigris::states() %>% 
  filter(GEOID != 69 & GEOID != 66 & GEOID != 60 & GEOID != 72 & GEOID != 78 & GEOID != 2) %>% 
  st_transform(terra::crs(gc_polygons)) %>% 
  vect()

# get Maricopa County boundary  [terra object]
maricopa_county <- tigris::counties(state = "Arizona") %>% 
  # filter(NAME == "Maricopa" | NAME == "Pinal" | NAME == "Pima" | NAME == "Santa Cruz") %>%  # set this to the desired counties
  filter(NAME == "Maricopa") %>%  # set this to the desired counties
  sf::st_transform(terra::crs(gc_polygons)) %>% 
  vect()

# census tracts
maricopa_county_census_tracts <- tigris::tracts(state = "Arizona") %>%
  filter(COUNTYFP == "013") %>%
  sf::st_transform(terra::crs(gc_polygons)) %>% 
  vect()

# explore boundaries of cities 
b1 <- st_read("data/boundary_data/incorporated_city_boundaries/Cities.shp")
b2 <- st_as_sf(b1) %>% 
  sf::st_transform(terra::crs(gc_polygons)) 
b3 <- st_make_valid(b2) %>% 
  vect()

########################################################################### LOAD landscape metric data #########################################################################


# read the golf course-level landscape metric data (MAG_EDA_4.R)
golf_course_scale_metrics <- readRDS(file= "data/rda_files/golf_course_scale_metrics.Rda")

# I calculated the golf course patch level metrics from the gc class data reclassified so that every golf course is the same class.
# the total golf course area is the same for both datasets, which is a good sign.

gc_patch_landscape_scale_metrics <- readRDS(file= "data/rda_files/gc_patch_landscape_scale_metrics.Rda")
# these metrics belong in a table


########################################################################### global cosmetic vars #########################################################################

gc_green = "#32CD32"

########################################################################### vector data plots #########################################################################

# visualize the golf courses within Maricopa County
png('photos/gc_spatial_analysis_EDA/MC_golf_courses.png', width = 2800, height = 2000, units='px', res = 300)
plot(maricopa_county)
lines(gc_polygons, col = "dark green")
dev.off()

# these are the incorporated city boundaries taken from somewhere online
png('photos/gc_spatial_analysis_EDA/gc_city_boundaries.png', width = 2800, height = 2000, units='px', res = 300)
plot(maricopa_county)
lines(b3)
lines(gc_polygons, col = "dark green")
dev.off()

# golf courses on top of census tracts
x <- ext(-12555628, -12421376, 3922353, 4026458)
png('photos/gc_spatial_analysis_EDA/gc_census_tracts.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(maricopa_county_census_tracts, x))
lines(maricopa_county)
lines(gc_polygons, col = "dark green")
dev.off()

#### @@@study site fig ####
png('photos/final_figures/study_site.png', width = 1900, height = 1620, units='px', res = 300)
plot(crop(b3, maricopa_county), col = "red", alpha = 0.5, xlim = c(-12630000, -12350000), ylim = c(3820000, 4050000))
lines(maricopa_county, lwd = 2)
lines(phx_ama, col = "blue", lwd = 1.6)
points(crop(gc_pts, maricopa_county), col = "dark green", cex = 1.3)
sbar(20000, xy = c(-12400000,3830000))
north(type=1, cex=.8, xy = c(-12375000,3860000))
dev.off()

# plots the continental united states
# png('lower48.png', width = 2800, height = 2000, units='px', res = 200)
# plot(cont_us) # specify the colors here.
# sbar(xy = "bottomleft", divs = 4)
# # sbar(10, c(6.2, 50.1), type="bar", cex=.8, divs=4)
# north(type=1)
# dev.off()

#### . ####
########################################################################### Scale of Analysis Fig. #########################################################################

# TODO Re-address this figure

gc_polygons_sf %>% 
  View()

ext_1 <- ext(-12516500, -12511100, 3980000, 3986000)
analysis_scale <- crop(gc_polygons, ext_1)

analysis_scale_sf <- st_as_sf(analysis_scale) %>% 
  filter(gc_name != "great_eagle_golf_club" & gc_name != "scw_trailridge" & gc_id != "121") %>% 
  sf::st_transform(terra::crs(gc_polygons)) %>% 
  vect()

# gc class level
png('photos/gc_spatial_analysis_EDA/vect/scale_of_analysis/gc_class_level.png', width = 2000, height = 2000, units='px', res = 300)
plot(analysis_scale_sf, axes = FALSE, col = c(gc_green, gc_green, gc_green, gc_green, gc_green, gc_green, gc_green, gc_green, gc_green))
box() # adds bounds
dev.off()

# individual gc level
sf.colors(n = 3)
png('photos/gc_spatial_analysis_EDA/vect/scale_of_analysis/gc_ind_level.png', width = 2000, height = 2000, units='px', res = 300)
plot(analysis_scale_sf, axes = FALSE, col = c("#0000B3FF", "#0000B3FF", "#0000B3FF", "#0000B3FF", "#0000B3FF", "#E53CC3FF", "#E53CC3FF", "#FFF50AFF", "#FFF50AFF"))
box() # adds bounds
dev.off()

# gc patch level
sf.colors(n = 9)
rainbow(9)
png('photos/gc_spatial_analysis_EDA/vect/scale_of_analysis/gc_patch_level.png', width = 2000, height = 2000, units='px', res = 300)
plot(analysis_scale_sf, axes = FALSE, col = c("#FF00AA", "#00AAFF", "#AAFF00", "#AA00FF", "#00FFAA", "#FFAA00", "#0000FF", "#00FF00", "#FF0000"))
box() # adds bounds
dev.off()

#### . ####
########################################################################### Buffer Analysis #########################################################################


#### @rasterize the data ####

# r <- rast(MC_LUCC, ncols=50000, nrows=41000)
# MC_rast_5m <- rasterize(MC_LUCC, r, "LONG_DISPL")

r <- rast(MC_LUCC, ncols=8510, nrows=6850)
MC_rast_30m <- rasterize(MC_LUCC, r, "LONG_DISPL")
MC_rast_30m

r <- rast(MC_LUCC, ncols=4250, nrows=3400)
MC_rast_60m <- rasterize(MC_LUCC, r, "LONG_DISPL")

r <- rast(MC_LUCC, ncols=2120, nrows=1700)
MC_rast_120m <- rasterize(MC_LUCC, r, "LONG_DISPL")

r <- rast(MC_LUCC, ncols=1405, nrows=1130)
MC_rast_180m <- rasterize(MC_LUCC, r, "LONG_DISPL")

r <- rast(MC_LUCC, ncols=850, nrows=685)
MC_rast_300m <- rasterize(MC_LUCC, r, "LONG_DISPL")

#### @reclassification ####
# key for reclassification VERSION 1
# 1 built environment: (Airport, Business Park, Cemetery, Commercial High, Commercial Low, Developing Employment, Developing Residential, Educational, 
#                  Industrial, Medical/Nursing Home, Mixed Use, Multi Family, Office, Other Employment, Railroads, Religious/Institutional,
#                  Single Family High Density, Single Family Low Density, Single Family Medium Density, Solar Generating Stations, Tourist Accomodations,
#                  Transportation, Public/Special Event/Military)
# 2 agriculture category: (Abandoned Agriculture, Agriculture, Dairy or Feedlot, Orchard)
# 3 desert park or preserve: (Desert Parks and Preserves)
# 4 golf course: (Golf Course)
# 5 non-golf course urban greenspace: (Active Open Space)
# 6 other (Public Land, Vacant, Vacant State Trust, Wash, Passive/Restricted Open Space))
# 7 water (Water)

# reclassification version 1
m_2 <- c(0, 2,
         1, 5,
         2, 2,
         3, 1,
         4, 1,
         5, 1,
         6, 1,
         7, 1,
         8, 2,
         9, 3,
         10, 1,
         11, 1,
         12, 1,
         13, 4,
         14, 1,
         15, 1,
         16, 1,
         17, 1,
         18, 1,
         19, 2,
         20, 1,
         21, 6,
         22, 6,
         23, 1,
         24, 1,
         25, 1,
         26, 1,
         27, 1,
         28, 1,
         29, 1,
         30, 1,
         31, 1,
         32, 6,
         33, 6,
         34, 6,
         35, 7)

rclmat_2 <- matrix(m_2, ncol = 2, byrow = TRUE)

# rast_GCall_5m_MC <- classify(MC_rast_5m, rclmat_2)
rast_GCall_30m_MC <- classify(MC_rast_30m, rclmat_2)
rast_GCall_60m_MC <- classify(MC_rast_60m, rclmat_2)
rast_GCall_120m_MC <- classify(MC_rast_120m, rclmat_2)
rast_GCall_180m_MC <- classify(MC_rast_180m, rclmat_2)
rast_GCall_300m_MC <- classify(MC_rast_300m, rclmat_2)

cover <- c("built_environment", "agriculture", "desert_park_or_preserve", "golf_course", "urban_green_space", "desert", "water")
d <- data.frame(id=1:7, cover = cover, value = 1:7)

# levels(rast_GCall_5m_MC) <- d
levels(rast_GCall_30m_MC) <- d
levels(rast_GCall_60m_MC) <- d
levels(rast_GCall_120m_MC) <- d
levels(rast_GCall_180m_MC) <- d
levels(rast_GCall_300m_MC) <- d

pal_reclassified <- c("#CC3363", "#F7B538", "#4A7B9D", "#7DD181", "#4B7F52", "#EF946C", "#20063B")
# built environment: #CC3363
# agriculture: #F7B538
# desert park: #4A7B9D
# golf course: #7DD181
# urban  green space:  #4B7F52
# other: #EF946C
# water: #20063B


#### @create buffers ####

# large sun city Extent
e_sc_large <- ext(-12520000, -12480000, 3960000, 3990000)
# Sun City Subset Extent
e_sc_small <- ext(-12520000, -12510000, 3980000, 3988000)

buff_1 <- terra::buffer(gc_polygons, 180)
buff_1_ag <- aggregate(buff_1)

buff_2 <- terra::buffer(gc_polygons, 2000)
buff_2_ag <- aggregate(buff_2)

#zoomed out
png('photos/gc_spatial_analysis_EDA/vect/buffer/buffer_180_MC.png', width = 2800, height = 2000, units='px', res = 300)
plot(gc_polygons, col = "dark green")
lines(buff_1_ag, col = "dark green")
dev.off()

# Sun City large test
png('photos/gc_spatial_analysis_EDA/vect/buffer/buffer_180_SCL.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_polygons, e_sc_large), col = "dark green")
lines(crop(buff_1_ag, e_sc_large), col = "dark green")
dev.off()

# Sun City small test
png('photos/gc_spatial_analysis_EDA/vect/buffer/buffer_180_SCS.png', width = 2800, height = 2000, units='px', res = 300)
plot(crop(gc_polygons, e_sc_small), col = "dark green")
lines(crop(buff_1_ag, e_sc_small), col = "dark green")
dev.off()

# mask a raster with the buffer
png('photos/gc_spatial_analysis_EDA/rast/buffer/buffer_180_SCS.png', width = 2800, height = 2000, units='px', res = 300)
mask_1 <- crop(rast_GCall_30m_MC,buff_1_ag, mask=TRUE)
plot(crop(mask_1, e_sc_small))
dev.off()

png('photos/gc_spatial_analysis_EDA/rast/buffer/buffer_180_MC.png', width = 2800, height = 2000, units='px', res = 300)
plot(mask_1)
dev.off()

png('photos/gc_spatial_analysis_EDA/rast/buffer/buffer_2000_SCS.png', width = 2800, height = 2000, units='px', res = 300)
mask_2 <- crop(rast_GCall_30m_MC,buff_2_ag, mask=TRUE)
plot(crop(mask_2, e_sc_small))
dev.off()

png('photos/gc_spatial_analysis_EDA/rast/buffer/buffer_2000_MC.png', width = 2800, height = 2000, units='px', res = 300)
plot(mask_2)
dev.off()

# reclassify to get rid of the golf course class (reclassify @180 m buffer)

m_3 <- c(1,1,
         2,2,
         3,3,
         4,NA,
         5,4,
         6,5,
         7,6)

rclmat_3 <- matrix(m_3, ncol = 2, byrow = TRUE)
buffer_complete_180 <- classify(mask_1, rclmat_3)

cover <- c("built_environment", "agriculture", "desert_park_or_preserve", "urban_green_space", "desert", "water")
d <- data.frame(id=1:6, cover = cover, value = 1:6)
levels(buffer_complete_180) <- d

# reclassify to get rid of the golf course class (reclassify @2000m buffer)
rclmat_3 <- matrix(m_3, ncol = 2, byrow = TRUE)
buffer_complete_2000 <- classify(mask_2, rclmat_3)

cover <- c("built_environment", "agriculture", "desert_park_or_preserve", "urban_green_space", "desert", "water")
d <- data.frame(id=1:6, cover = cover, value = 1:6)
levels(buffer_complete_2000) <- d

plot(buffer_complete_180)
plot(buffer_complete_2000)

#### @lm analysis of buffers ####
# composition analysis for the 1800m buffer
buffer_1_composition <- calculate_lsm(buffer_complete_180, what = c("lsm_c_ca", "lsm_c_pland"))%>%
  mutate(resolution = "30m") %>% 
  mutate(buffer_type = "180m")

# composition analysis for the 2000 m buffer
buffer_2_composition <- calculate_lsm(buffer_complete_2000, what = c("lsm_c_ca", "lsm_c_pland"))%>%
  mutate(resolution = "30m") %>% 
  mutate(buffer_type = "2000m")

buffer_df <- buffer_1_composition %>% 
  rbind(buffer_2_composition)

#### @@@ buffer fig ####
# setwd('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures')
# landscape_pattern_path <- "/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures"

cover_colors <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")
legend_labels = c("1" = "Built environment", "2" = "Agriculture", "3" = "Regional park", "4" = "Urban green space", "5" = "Desert", "6" = "Water")

hex <- hue_pal()(6)
show_col(hex)

buffer_fig <- buffer_df %>% 
  filter(metric == "pland") %>% 
  mutate(class = factor(class)) %>% 
  mutate(buffer_type = factor(buffer_type, levels = c("180m", "2000m"))) %>% 
  ggplot(aes(x = buffer_type, y = value, fill = class)) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values = cover_colors, name = "Land use", labels = legend_labels) +
  xlab("Buffer distance") + ylab("% Cover of surrounding land uses") + theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0), size = 10),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    # legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black")) 

buffer_fig

# setwd('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures')
# landscape_pattern_path <- "/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures"

ggsave(filename = "photos/final_figures/buffer_fig.png", width = 6, height = 4, scale = 0.9)

#### . ####
########################################################################### GC-level analysis #########################################################################

lm_violin_color = "#805D93"

#### @CA ####
gc_class_area_fig <- golf_course_scale_metrics %>% # 426 patches
  filter(metric == "ca") %>% 
  ggplot(aes(x = 1, y = value)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.4) + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 60),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

gc_class_area_fig

ggsave(filename = "photos/final_figures/patch_visualizations/CA.png", width = 8, height = 6, scale = 1.0)


#### @NP ####
gc_class_np_fig <- golf_course_scale_metrics %>% # 426 patches
  filter(metric == "np") %>% 
  ggplot(aes(x = 1, y = value)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.4) + scale_y_continuous(limits = c(0,15), breaks = c(0,3,6,9,12,15)) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 60),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))


gc_class_np_fig

ggsave(filename = "photos/final_figures/patch_visualizations/NP.png", width = 8, height = 6, scale = 1.0)

#### @LSI#####
gc_class_lsi_fig <- golf_course_scale_metrics %>% # 426 patches
  filter(metric == "lsi") %>% 
  ggplot(aes(x = 1, y = value)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.4) + scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8)) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 60),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))


gc_class_lsi_fig

ggsave(filename = "photos/final_figures/patch_visualizations/LSI.png", width = 8, height = 6, scale = 1.0)

#### @AI ####
gc_class_ai_fig <- golf_course_scale_metrics %>% # 426 patches
  filter(metric == "ai") %>% 
  ggplot(aes(x = 1, y = value)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.4) + scale_y_continuous(limits = c(65,110), breaks = c(70,80,90,100,110)) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 60),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))


gc_class_ai_fig

ggsave(filename = "photos/final_figures/patch_visualizations/AI.png", width = 8, height = 6, scale = 1.0)


#### @LPA ####
gc_LPA_fig <- golf_course_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_id" = "class") %>%
  select(gc_id, value) %>%
  group_by(gc_id) %>%
  summarise(LPA = max(value)) %>%
  ggplot(aes(x = 1, y = LPA)) + geom_boxplot(width=0.1) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.4) + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 60),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.position="none",
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

gc_LPA_fig

ggsave(filename = "photos/final_figures/patch_visualizations/LPA.png", width = 8, height = 6, scale = 1.0)

#### @violin plot demonstration ####

gc_class_area_dotplot <- golf_course_scale_metrics %>% # 426 patches
  filter(metric == "ca") %>% 
  ggplot(aes(x = 1, y = value)) + geom_boxplot(width=0.1, lwd = 2.4) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.4, shape = 3, col = "#000000")  + ylab("Golf course area (ha)") + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + theme(
                 axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10), size = 16),
                 axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 10), size = 14),
                 strip.text.x = element_text(size = 14),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 panel.border = element_blank(),
                 legend.background = element_rect(colour = NA),
                 legend.position="none",
                 legend.key = element_rect(colour = "white", fill = NA),
                 axis.line = element_line(colour = "black"))

gc_class_area_dotplot

ggsave(filename = "photos/gc_spatial_analysis_EDA/example_violin/lm_boxplot.png", width = 6, height = 6, scale = 1.2)










#### . ####
########################################################################### INEQUALITY ANALYSIS #########################################################################

# setwd('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/MAG_EDA_4/environmental_justice')
# landscape_pattern_path <- "/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/MAG_EDA_4/environmental_justice"

#### @prep data ####

# gc_temp is an sf object containing all golf course vectors

# census tracts in Maricopa County [sf object]
census_tracts <- tigris::tracts(state = "Arizona", year = "2020") %>%
  filter(COUNTYFP == "013") %>%
  sf::st_transform(terra::crs(gc_polygons)) 

# list with rows corresponding to census tracts that overlap with golf course boundaries
m <- st_intersects(census_tracts, gc_polygons_sf, sparse = TRUE)
myvec <- sapply(m, NROW)
myvec

# create the gc_present column, which has a 1 if a golf course is present and a 0 if it does not.
gc_present_column <- data.frame(myvec <- sapply(m, NROW)) %>% 
  rename("gc_present" = 1) %>%
  mutate(gc_present = ifelse(gc_present == 0, 0, 1)) 

# Another source for income data
# https://data.census.gov/table?q=b19013&tid=ACSDT1Y2018.B19013&hidePreview=true



# https://api.census.gov/data/2016/acs/acs1/groups/B19013.html
# tidycensus to get median household income data
median_household_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", # median household income in the past 12 months
  state = "AZ", 
  county = "Maricopa",
  year = 2020,
  geometry = TRUE
) %>% 
  st_drop_geometry() 

pop <- get_acs(
  geography = "tract", 
  variables = "S0101_C01_001E", # pop data for the past 12 months
  state = "AZ", 
  county = "Maricopa",
  year = 2020,
  geometry = TRUE
) %>% 
  st_drop_geometry() 

# this df has household income and gc presence
polygon_attributes_df <- census_tracts %>% 
  cbind(gc_present_column) %>% 
  merge(., median_household_income, by = "GEOID")

# get rid of the geometry so the columns can be used in the correlation analysis
correlation_df <- polygon_attributes_df %>% 
  na.omit() %>% 
  st_drop_geometry %>% 
  select(estimate, gc_present)

#### @correlation analysis ####
# the distribution of the continuous variable is mostly normal, which as an assumption for the point-biserial correlation
correlation_df %>% 
  ggplot(aes(x = estimate)) + geom_histogram(bins=40)

cor.test(correlation_df$estimate, correlation_df$gc_present)

#### @figures ####

#### @@@census/gc boundaries ####
# figure showing golf course boundaries and census tracts [based on the terra versions of the data]
# TODO manually set the x,y axis labels here?
png('photos/gc_spatial_analysis_EDA/environmental_justice/vect_golf_courses_census_tracts.png', width = 2300, height = 1855, units='px', res = 300)
plot(crop(maricopa_county_census_tracts, x), lwd = 0.6, xlim = c(-12555628, -12421376), ylim = c(3922353, 4026458)) # -12555628, -12421376, 3922353, 4026458
lines(gc_polygons, col = "dark green")
lines(maricopa_county, lwd = 6)
dev.off()

#### @@@chloropeth/points ####
# chorpleth map showing median income (continuous scale) with census tracts containing golf courses having centroids [based on the sf objects]
# https://stackoverflow.com/questions/49180763/adding-a-point-to-a-choropleth-map-based-on-lat-and-long-r

png('photos/gc_spatial_analysis_EDA/environmental_justice/estimate.png', width = 2300, height = 1855, units='px', res = 300)
plot(polygon_attributes_df["estimate"])
dev.off()

png('photos/gc_spatial_analysis_EDA/environmental_justice/gc_present.png', width = 2300, height = 1855, units='px', res = 300)
plot(polygon_attributes_df["gc_present"])
dev.off()

# change the point for the census tract in the western tract
# this makes it so the extent is smaller and you can more easily see the detail of each census tract
adjusted_point <- polygon_attributes_df %>% 
  filter(GEOID == "04013040515") %>% 
  st_drop_geometry() %>% # get rid of the geometry for this observation
  mutate(geometry = st_sfc(st_point(c(-12551400,4004648)))) %>% # add the new geometry, which moves the point to the right
  st_as_sf(., crs = terra::crs(polygon_attributes_df)) # transform into an sf object with a matching crs

# contains all of the point data for census tracts containing golf course boundaries
troids <- st_point_on_surface(polygon_attributes_df) %>% 
  filter(gc_present == 1) %>% 
  filter(GEOID != "04013040515") %>% 
  rbind(adjusted_point)

# convert to raster
rast_polygon_attributes_df <- polygon_attributes_df %>% 
  vect()

# convert to raster
rast_troids <- troids %>% 
  vect()

# setwd('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures')
# landscape_pattern_path <- "/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures"

# x <- ext(-12555628, -12421376, 3922353, 4026458)

png('photos/final_figures/troids.png', width = 2300, height = 1620, units='px', res = 300)
plot(crop(rast_polygon_attributes_df, x), "estimate", type = "continuous", lwd = 0.6, col = plasma(100, direction = -1), xlim = c(-12555628, -12421376), ylim = c(3922353, 4026458), pax = list(sides = c(3,4), labels = T), main = "", plg = list(loc = "left", ext = c(-12564028, -12560028, 3922353, 4026458)))
lines(maricopa_county, lwd = 6)
points(rast_troids, cex=1, col = "#003200", pch = 16)
lines(crop(freeways, x), lwd = 1.2, lty = 2, col = "#D3D3D3")
sbar(10000, xy = c(-12520000,3925000))
north(type=1, cex=.8, xy = c(-12524000,3932000))
dev.off()

png('photos/final_figures/troids_out.png', width = 2300, height = 1620, units='px', res = 300)
plot(rast_polygon_attributes_df, "estimate", type = "continuous", lwd = 0.6, col = plasma(100, direction = -1), pax = list( 
  sides = F,
  labels = F,
  tick = F))
lines(maricopa_county, lwd = 6)
dev.off()

#### @@@golf course frequency ####
name_id_to_join <- gc_polygons_sf %>%
  st_drop_geometry() %>% 
  select(gc_id, gc_name_id, gc_name, course)

status_to_join <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_name, status) %>% 
  unique()

# poverty thresholds by 2023
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
# 2 persons/household $19720
# 3 persons/household $24860  [$25000] 
# The bin width ($25,000 NHHI) and number of bins (9) were set by the first bin [0-$25000].
# $25000 is the poverty threshold for a 3-person household [average household size in MC is 2.6] https://www.census.gov/quickfacts/fact/table/maricopacountyarizona/PST045222
# The width was carried across the entire range of MHHI--giving 9 total bins.

hex <- hue_pal()(6)
show_col(hex)

b = 9

# this histogram is golf course count. In other words, it is not counting the number of census tracts with some characteristic.
golf_course_frequency <- st_join(gc_pts_sf, census_tracts, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>% 
  select(gc_id, NAMELSAD, estimate, moe) %>%
  rename("MHHI_estimate" = "estimate") %>%
  merge(., name_id_to_join, by = "gc_id") %>%
  merge(., status_to_join, by = "gc_name") %>%
  unique() %>% 
  select(gc_name, course, gc_id, gc_name_id, status, MHHI_estimate, moe, NAMELSAD) %>% 
  mutate(status = factor(status)) %>% 
  ggplot(aes(x = MHHI_estimate, fill = status)) + geom_histogram(bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), position = "stack", color = "black") + 
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250")) + 
  scale_fill_manual(name = "Golf course type", values = c("#35b779", "#fde725", "#31688e", "#440154")) + scale_y_continuous(limits = c(0,80), breaks = c(0,20,40,60,80)) + xlab("Census tract MHHI estimate (thousand $USD)") + ylab("Golf course \n frequency") + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

golf_course_frequency

ggsave(filename = "photos/final_figures/troids_golf_course_frequency.png", width = 6, height = 2, scale = 1.7)

# whirlwind at white horse pass is removed because it does not have a MHHI estimate.

#### @@@golf course relative frequency ####
rf_func <- function(count, status) {
  
  pop_bin_sequence <- pop %>% 
    merge(.,median_household_income, by = "NAME") %>% 
    rename("pop_estimate" = "estimate.x") %>% 
    rename("MHHI_estimate" = "estimate.y") %>% 
    mutate(bin_pop = case_when((MHHI_estimate > 0 & MHHI_estimate < 25000) ~ "bin1",
                               (MHHI_estimate > 25000 & MHHI_estimate < 50000) ~ "bin2",
                               (MHHI_estimate > 50000 & MHHI_estimate < 75000) ~ "bin3",
                               (MHHI_estimate > 75000 & MHHI_estimate < 100000) ~ "bin4",
                               (MHHI_estimate > 100000 & MHHI_estimate < 125000) ~ "bin5",
                               (MHHI_estimate > 125000 & MHHI_estimate < 150000) ~ "bin6",
                               (MHHI_estimate > 150000 & MHHI_estimate < 175000) ~ "bin7",
                               (MHHI_estimate > 175000 & MHHI_estimate < 200000) ~ "bin8",
                               (MHHI_estimate > 200000 & MHHI_estimate < 225000) ~ "bin9",
                               (MHHI_estimate > 225000 & MHHI_estimate < 250000) ~ "bin10",
                               .default =  "na_value_for_MHHI")) %>% 
    filter(bin_pop != "na_value_for_MHHI") %>% # gets rid of census tracts with no MHHI estimate
    select(pop_estimate, bin_pop) %>% 
    group_by(bin_pop) %>%
    summarise(total_pop = sum(pop_estimate)) 
  
  pop_df <- pop_bin_sequence %>% 
    rbind(pop_bin_sequence) %>% 
    rbind(pop_bin_sequence) %>% 
    rbind(pop_bin_sequence) 
  
  d <- data.frame(count, status) %>% 
    cbind(pop_df) %>% 
    mutate(relative_count = count/total_pop) %>% 
    pull(relative_count)
  
  # # to get the magnitude of difference between private courses in 25,000-125,000 and 125,000 and 225,000
  # data.frame(count, status) %>% 
  #   cbind(pop_df) %>% 
  #   mutate(relative_count = count/total_pop) %>% 
  #   filter(status == "private") %>%
  #   filter(bin_pop != "bin1") %>% 
  #   View()
  
  # # to get the magnitude of difference between total courses in 25,000-125,000 and 125,000 and 225,000
  # data.frame(count, status) %>%
  #   cbind(pop_df) %>%
  #   mutate(relative_count = count/total_pop) %>%
  #   group_by(bin_pop) %>% 
  #   summarise(mean = mean(relative_count)) %>% 
  #   filter(bin_pop != "bin1") %>%
  #   View()
  
}

# + scale_y_continuous(limits = c(0,80), breaks = c(0,20,40,60,80))

golf_course_relative_frequency <- st_join(gc_pts_sf, census_tracts, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>% 
  select(gc_id, NAMELSAD, estimate, moe) %>%
  rename("MHHI_estimate" = "estimate") %>%
  merge(., name_id_to_join, by = "gc_id") %>%
  merge(., status_to_join, by = "gc_name") %>%
  unique() %>% 
  select(gc_name, course, gc_id, gc_name_id, status, MHHI_estimate, moe, NAMELSAD) %>% 
  mutate(status = factor(status)) %>% 
  ggplot(aes(x = MHHI_estimate, fill = status)) + geom_histogram(aes(y = after_stat(rf_func(count, fill)), position = "stack"), bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), color = "black") + 
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250")) + scale_y_continuous(limits = c(0,0.00025), breaks = c(0,0.00005, 0.00010, 0.00015, 0.00020, 0.00025), labels = c("0", expression('5e' ^-5), expression('10e' ^-5), expression('15e' ^-5), expression('20e' ^-5), expression('25e' ^-5))) + 
  scale_fill_manual(name = "Golf course type", values = c("#35b779", "#fde725", "#31688e", "#440154")) + xlab("Census tract MHHI estimate (thousand $USD)") + ylab("Per capita \n golf course \n frequency") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0), size = 18),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 20, l = 20), size = 16),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

golf_course_relative_frequency

ggsave(filename = "photos/final_figures/troids_golf_course_relative_frequency.png", width = 6, height = 2, scale = 1.7)

#### @@@census tracts frequency ####
# this figure is counting the number of census tracts in each bin with golf courses (broken down into golf course type) and without golf courses.

# this dataset has census tracts with golf courses
# NOTE: there are census tracts with multiple golf course types here. So census tracts are counted more than one time.
tracts_with_gcs <- st_join(gc_pts_sf, census_tracts, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>%
  select(gc_id, NAMELSAD, estimate, moe) %>%
  rename("MHHI_estimate" = "estimate") %>%
  merge(., name_id_to_join, by = "gc_id") %>%
  merge(., status_to_join, by = "gc_name") %>%
  unique() %>%
  select(NAMELSAD, MHHI_estimate, status) %>%
  unique() 

# # census tracts with multiple gc types
# tracts_with_gcs %>% 
#   select(NAMELSAD) %>% 
#   count(NAMELSAD) %>% 
#   filter(n > 1) %>% 
#   View()

# this dataset has census tracts without golf courses
# 1,022 golf courses is more than the number of census tracts because some tracts have multiple golf course types.
census_tracts_frequency <- census_tracts %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>% 
  rename("MHHI_estimate" = "estimate") %>%
  filter(!NAMELSAD %in% tracts_with_gcs$NAMELSAD) %>%
  unique() %>% 
  select(NAMELSAD, MHHI_estimate) %>%
  mutate(status = "no_golf_course") %>%
  rbind(tracts_with_gcs) %>% 
  unique() %>% 
  mutate(status = factor(status)) %>% 
  ggplot(aes(x = MHHI_estimate, fill = status)) + geom_histogram(bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), position = "stack", color = "black") + 
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250"))  + 
  scale_fill_manual(name = "Golf course status", values = c("#e5e5e5", "#35b779", "#fde725", "#31688e", "#440154")) + scale_y_continuous(limits = c(0,400), breaks = c(0,100,200,300,400)) + xlab("Census tract MHHI estimate (thousand $USD)") + ylab("Census tract \n frequency") +
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 18),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

census_tracts_frequency

ggsave(filename = "photos/final_figures/troids_census_tracts_frequency.png", width = 6, height = 2, scale = 1.7)



#### @@@census tracts relative frequency ####
# https://stackoverflow.com/questions/69210524/show-percent-in-ggplot-histogram
# https://stackoverflow.com/questions/68227541/ggplot-geom-bar-plot-percentages-by-group-and-facet-wrap/68227836#68227836

my_func <- function(count, status) {
  
  status_1 <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9")
  status_2 <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9")
  status_3 <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9")
  status_4 <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9")
  status_5 <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9")
  
  bins <- c(status_1, status_2, status_3, status_4, status_5)
  
  d <- data.frame(count, status, bins) %>%
    group_by(bins) %>% 
    summarise(sum = sum(count))
  
  bin_sums <- d$sum
  
  # data.frame(count, status, bins, bin_sums) %>%
  #   group_by(status) %>%
  #   mutate(pct = count / sum(count)) %>%
  #   View()
  
  
  data.frame(count, status, bins) %>%
    # group_by(status) %>%
    # mutate(pct = count / sum(count)) %>%
    mutate(pct = count / bin_sums) %>% 
    pull(pct)
  
}

census_tracts_relative_frequency <- census_tracts %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>% 
  rename("MHHI_estimate" = "estimate") %>%
  filter(!NAMELSAD %in% tracts_with_gcs$NAMELSAD) %>%
  unique() %>% 
  select(NAMELSAD, MHHI_estimate) %>%
  mutate(status = "no_golf_course") %>%
  rbind(tracts_with_gcs) %>% 
  unique() %>% 
  mutate(status = factor(status)) %>% 
  ggplot(aes(MHHI_estimate, fill = status)) + geom_histogram(aes(y = after_stat(my_func(count, fill)), position = "stack"), bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), color = "black") + 
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250"))  + 
  scale_fill_manual(name = "Golf course status", values = c("#e5e5e5", "#35b779", "#fde725", "#31688e", "#440154")) + scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0)) + xlab("Census tract MHHI estimate (thousand $USD)") + ylab("Census tracts (%)") +
  theme(
    
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 18),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

census_tracts_relative_frequency

ggsave(filename = "photos/final_figures/troids_census_tracts_relative_frequency.png", width = 6, height = 2, scale = 1.7)

# num census tracts per bin
census_tracts %>% 
  st_drop_geometry() %>% 
  merge(., median_household_income, by = "GEOID") %>% 
  rename("MHHI_estimate" = "estimate") %>%
  filter(!NAMELSAD %in% tracts_with_gcs$NAMELSAD) %>%
  unique() %>% 
  select(NAMELSAD, MHHI_estimate) %>%
  mutate(status = "no_golf_course") %>%
  rbind(tracts_with_gcs) %>% 
  unique() %>% 
  mutate(bin_frequency = case_when((MHHI_estimate > 0 & MHHI_estimate < 25000) ~ "bin1",
                                   (MHHI_estimate > 25000 & MHHI_estimate < 50000) ~ "bin2",
                                   (MHHI_estimate > 50000 & MHHI_estimate < 75000) ~ "bin3",
                                   (MHHI_estimate > 75000 & MHHI_estimate < 100000) ~ "bin4",
                                   (MHHI_estimate > 100000 & MHHI_estimate < 125000) ~ "bin5",
                                   (MHHI_estimate > 125000 & MHHI_estimate < 150000) ~ "bin6",
                                   (MHHI_estimate > 150000 & MHHI_estimate < 175000) ~ "bin7",
                                   (MHHI_estimate > 175000 & MHHI_estimate < 200000) ~ "bin8",
                                   (MHHI_estimate > 200000 & MHHI_estimate < 225000) ~ "bin9",
                                   (MHHI_estimate > 225000 & MHHI_estimate < 250000) ~ "bin10",
                                   .default =  "na_value_for_MHHI")) %>% 
  count(bin_frequency) %>% 
  View()

#### combined fig ####
(golf_course_frequency) + (golf_course_relative_frequency) + plot_layout(ncol = 1, nrow = 2) + plot_annotation(tag_levels = "a") & theme(
  plot.tag = element_text(size = 18),
  plot.subtitle = element_text(size = 10))

ggsave(filename = "photos/final_figures/troid_hist_combined.png", width = 6, height = 3.3, scale = 1.7)

#### @@@num_gc_fig ####
# this histogram has the count of census tracts with golf courses in them on the y-axis.
troid_hist_colors <- c("#f0f921", "#fdca26", "#fb9f3a", "#ed7953", "#d8576b", "#bd3786", "#9c179e", "#7201a8", "#46039f")

correlation_df %>% 
  filter(gc_present == 1) %>% 
  ggplot(aes(x = estimate)) + geom_histogram(bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), fill = troid_hist_colors) + 
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250")) + 
  xlab("Median household income (thousand $USD)") + ylab("Golf course \n frequency") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 19),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 19),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))


#### @@@ kernel desnity fig ####
kernel_density_vis <- correlation_df %>% 
  filter(gc_present == 1) %>% 
  ggplot(aes(x = estimate)) + geom_histogram(bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000), fill = troid_hist_colors) + 
  geom_density(aes(y=25000 * ..count..), linewidth = 3)+
  scale_x_continuous(limits = c(0,250000), breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225","250")) + 
  xlab("Median household income (thousand $USD)") + ylab("Golf course \n frequency") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 19),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 19),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

kernel_density_vis

ggsave(filename = "photos/final_figures/kernel_density_vis.png", width = 6, height = 4, scale = 1.2)


non_gc_tracts <- correlation_df %>% 
  filter(gc_present == 0) %>% 
  ggplot(aes(x = estimate)) + geom_histogram(bins = b, breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000)) + scale_x_continuous(limits = c(0,250000)) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 14),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 16),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 14),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

non_gc_tracts



# TODO
# average household size in Phoenix? That will give our poverty line estimate. That will set the interval, which can set the number of bins.

#todo set the same X-axis.

#### @@@stripchart ####
# TODO format this better (see resources online)
# the stripchart shows homogeneity of variance in the two dichotomous groups, which is an assumption of the test
stripchart(correlation_df$estimate ~ correlation_df$gc_present,
           method = "stack",
           main = "GC present (1) vs GC not present (0)"
) 

correlation_df %>% # this one shows the points on the violin plots.
  mutate(gc_present = factor(gc_present)) %>% 
  ggplot(aes(x = gc_present, y = estimate, fill = gc_present)) + 
  geom_violin(scale = "count") +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.2, shape = 3) + geom_boxplot(width=0.1) + scale_x_discrete(labels = c("0" = "Not present", "1" = "Present")) + 
  scale_y_continuous(limits = c(0,250000), breaks = c(0,50000,100000,150000,200000,250000), labels = c(0,50,100,150,200,250)) + xlab("Golf course presence \n within census tract") + ylab ("Median household income \n (Thousand USD$)") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 14),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 16),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 14),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

ggsave(filename = "photos/gc_spatial_analysis_EDA/example_violin/violin_plot.png", width = 6, height = 4.5, scale = 1.2)

# violin plot
correlation_df %>% 
  mutate(gc_present = factor(gc_present)) %>% 
  ggplot(aes(x = gc_present, y = estimate, fill = gc_present)) + 
  geom_violin(scale = "count") + 
  geom_boxplot(width=0.1, fill = "#FFFFFF") + scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
  scale_y_continuous(limits = c(0,250000), breaks = c(0,50000,100000,150000,200000,250000), labels = c(0,50,100,150,200,250)) +
  scale_fill_manual(values = c("#F8766D", "#00C094")) + 
  xlab("Golf course presence in census tract") + ylab ("Median household income \n (Thousand USD$)") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 16),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 14),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 16),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 20), size = 14),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    legend.position = "none",
    axis.line = element_line(colour = "black"))

ggsave(filename = "photos/gc_spatial_analysis_EDA/example_violin/violin_plot.png", width = 6, height = 4.5, scale = 1.2)


#### . ####
########################################################################### Patch Visualisation Fig #########################################################################
# this figure represents the extremes for golf course area, number of patches, and golf course shape

#### @get extremes ####

# titles to join
gc_titles <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, gc_name, course, num_holes) %>% 
  unique()

# course area
golf_course_scale_metrics %>% 
  rename("gc_id" = "class") %>% 
  filter(metric == "ca") %>% 
  merge(., gc_titles, by = "gc_id") %>%
  View()

# paradise_peak_gc is the 18-hole course with the smallest area
# tatum_ranch_gc
# devil claw at white horse pass is the 18-hole course with the largest area

# number of patches
golf_course_scale_metrics %>% 
  rename("gc_id" = "class") %>% 
  filter(metric == "np") %>% 
  merge(., gc_titles, by = "gc_id") %>% 
  View()

# tpc_scottsdale: stadium_course is tied for the least patches (1)
# ken_mcdonald_gc (3)
# arizona_biltmore_cc has the most patches (13)

# LSI
golf_course_scale_metrics %>% 
  rename("gc_id" = "class") %>% 
  filter(metric == "lsi") %>% 
  merge(., gc_titles, by = "gc_id") %>% 
  View()

# luke_falcon_dunes_gc
# bellair_golf_club
# los_caballeros_golf_club

# AI
golf_course_scale_metrics %>% 
  rename("gc_id" = "class") %>% 
  filter(metric == "ai") %>% 
  merge(., gc_titles, by = "gc_id") %>% 
  View()

# los_caballeros_golf_club
# royal_palms_gc
# luke_falcon_dunes_gc

# LPA
golf_course_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_id" = "class") %>%
  select(gc_id, value) %>%
  group_by(gc_id) %>%
  summarise(LPA = max(value)) %>%
  View()

# paradise_peak_gc 
# painted_mountain_gc
# whirlwind_at_wildhorse_pass: devil_claw

# all metrics for each course
lpa_to_join <- golf_course_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_id" = "class") %>%
  select(gc_id, value) %>%
  group_by(gc_id) %>%
  summarise(LPA = max(value))

gc_names_to_join <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, gc_name) %>% 
  unique()

golf_course_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_id" = "class") %>% 
  merge(., lpa_to_join, by = "gc_id") %>% 
  merge(., gc_names_to_join, by = "gc_id") %>% 
  View()


#### @get extents ####
# we need the extents for paradise_peak_gc, devil_claw @ whildhorse pass, stadium_course @ TPC scottsdale, links course @ arizona_biltmore, luke_falcon_dunes, arizona_golf_resort
# we need to get the centroids for each of these courses

gc_polygons_sf %>% 
  st_centroid() %>% 
  mutate(x_coord = unlist(map(geometry,1)),y_coord = unlist(map(geometry,2))) %>% # extract the coordinates
  mutate(gc_id = factor(gc_id)) %>%
  group_by(gc_id) %>%
  st_drop_geometry() %>% # get rid of the geometry so "summarise()" works
  summarise(across(c(x_coord, y_coord), mean)) %>%
  merge(., gc_titles, by = "gc_id") %>%
  filter((gc_name == "paradise_peak_gc") | (gc_name == "tatum_ranch_gc") | (gc_name == "whirlwind_at_wildhorse_pass" & course == "devil_claw") | (gc_name == "tpc_scottsdale" & course == "stadium_course") | (gc_name == "ken_mcdonald_gc") | (gc_name == "arizona_biltmore_cc" & course == "links")| (gc_name == "luke_falcon_dunes_gc") | (gc_name == "bellair_golf_club") | (gc_name == "los_caballeros_golf_club") | (gc_name == "royal_palms_gc") | (gc_name == "painted_mountain_gc")) %>%
  View()

# generates an extent when given an x,y centroid
generate_extent <- function(x,y, fixed_distance) {
  xmin = x - fixed_distance - 1500
  xmax = x + fixed_distance  
  ymin = y - fixed_distance
  ymax = y + fixed_distance
  
  extent = ext(xmin, xmax, ymin, ymax)
  return(extent)
}
# when distance is fixed at 1700, width = 2300, height = 2000
fixed_distance = 1400
w = 2500
h = 1500

# get the raster data
# the figure is based on landscape metrics, thus, raster data should be used.
r <- rast(gc_polygons, ncols=4463, nrows=3457)
gc_30m <- rasterize(gc_polygons, r, "gc_id")
gc_30m

#### @unique gc vis ####

#### @CA figs ####



# paradise_peak_west
ca_low_extreme = generate_extent(-12467640, 3988120, fixed_distance)
ca_low_extreme_cropped <- crop(gc_30m, ca_low_extreme)

png('photos/final_figures/patch_visualizations/ca_low_extreme.png', width = w, height = h, units='px', res = 300)
plot(ca_low_extreme_cropped, "gc_id", ext = ca_low_extreme, col = gc_green, pax = list( 
  labels = F,
  tick = F))
# sbar(500, xy = c(-12467340,3987120))
# north(type=1, cex=.8, xy = c(-12467640,3987120))
dev.off()

# scale_bar
png('photos/final_figures/patch_visualizations/ca_scale_bar.png', width = w, height = h, units='px', res = 300)
plot(ca_low_extreme_cropped, "gc_id", ext = ca_low_extreme, col = gc_green, pax = list( 
  labels = F,
  tick = F))
sbar(1000, xy = c(-12467640,3988720))
north(type=1, cex=.8, xy = c(-12467640,3987120))
dev.off()

# tatum ranch
ca_moderate = generate_extent(-12465876, 3996522, fixed_distance)
ca_moderate_cropped <- crop(gc_30m, ca_moderate)

png('photos/final_figures/patch_visualizations/ca_moderate.png', width = w, height = h, units='px', res = 300)
plot(ca_moderate_cropped, "gc_id", ext = ca_moderate, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# devils_claw
ca_high_extreme = generate_extent(-12466513, 3931940, fixed_distance)
ca_high_extreme_cropped <- crop(gc_30m, ca_high_extreme)

png('photos/final_figures/patch_visualizations/ca_high_extreme.png', width = w, height = h, units='px', res = 300)
plot(ca_high_extreme_cropped, "gc_id", ext = ca_high_extreme, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

#### @NP figs ####
# tpc scottsdale stadium
np_low_extreme = generate_extent(-12458405, 3980556, fixed_distance)
np_low_extreme_cropped <- crop(gc_30m, np_low_extreme)

png('photos/final_figures/patch_visualizations/np_low_extreme.png', width = w, height = h, units='px', res = 300)
plot(np_low_extreme_cropped, "gc_id", ext = np_low_extreme, col = c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# ken_mcdonald_gc
np_moderate = generate_extent(-12460694, 3942746, fixed_distance)
np_moderate_cropped <- crop(gc_30m, np_moderate)

png('photos/final_figures/patch_visualizations/np_moderate.png', width = w, height = h, units='px', res = 300)
plot(np_moderate_cropped, "gc_id", ext = np_moderate, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# biltmore links course
np_high_extreme = generate_extent(-12470254, 3964578, fixed_distance)
np_high_extreme_cropped <- crop(gc_30m, np_high_extreme)

png('photos/final_figures/patch_visualizations/np_high_extreme.png', width = w, height = h, units='px', res = 300)
plot(np_high_extreme_cropped, "gc_id", ext = np_high_extreme, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

#### @LSI figs ####
# luke falcon dunes
LSI_low_extreme = generate_extent(-12511078, 3969232, fixed_distance)
LSI_low_extreme_cropped <- crop(gc_30m, LSI_low_extreme)

png('photos/final_figures/patch_visualizations/LSI_low_extreme.png', width = w, height = h, units='px', res = 300)
plot(LSI_low_extreme_cropped, "gc_id", ext = LSI_low_extreme, col = gc_green, pax = list( 
  labels = F,
  tick = F))
dev.off()

# bellar_golf_club
lsi_moderate = generate_extent(-12485413, 3981224, fixed_distance)
lsi_moderate_cropped <- crop(gc_30m, lsi_moderate)

png('photos/final_figures/patch_visualizations/LSI_moderate.png', width = w, height = h, units='px', res = 300)
plot(lsi_moderate_cropped, "gc_id", ext = lsi_moderate, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# los caballeros
LSI_high_extreme = generate_extent(-12554329, 4020813, fixed_distance)
LSI_high_extreme_cropped <- crop(gc_30m, LSI_high_extreme)

png('photos/final_figures/patch_visualizations/LSI_high_extreme.png', width = w, height = h, units='px', res = 300)
plot(LSI_high_extreme_cropped, "gc_id", ext = LSI_high_extreme, col = c(gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

#### @AI figs ####
# los caballeros
AI_low_extreme =generate_extent(-12554329, 4020813, fixed_distance)
AI_low_extreme_cropped <- crop(gc_30m, AI_low_extreme)

png('photos/final_figures/patch_visualizations/AI_low_extreme.png', width = w, height = h, units='px', res = 300)
plot(AI_low_extreme_cropped, "gc_id", ext = AI_low_extreme, col = gc_green, pax = list( 
  labels = F,
  tick = F))
dev.off()

# royal_palms_gc
AI_moderate = generate_extent(-12445598, 3954898, fixed_distance)
AI_moderate_cropped <- crop(gc_30m, AI_moderate)

png('photos/final_figures/patch_visualizations/AI_moderate.png', width = w, height = h, units='px', res = 300)
plot(AI_moderate_cropped, "gc_id", ext = AI_moderate, col = c("#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# luke falcon dunes
AI_high_extreme = generate_extent(-12511078, 3969232, fixed_distance)
AI_high_extreme_cropped <- crop(gc_30m, AI_high_extreme)

png('photos/final_figures/patch_visualizations/AI_high_extreme.png', width = w, height = h, units='px', res = 300)
plot(AI_high_extreme_cropped, "gc_id", ext = AI_high_extreme, col = c(gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

#### @LPA figs ####
# los caballeros
LPA_low_extreme =generate_extent(-12554329, 4020813, fixed_distance)
LPA_low_extreme_cropped <- crop(gc_30m, LPA_low_extreme)

png('photos/final_figures/patch_visualizations/LPA_low_extreme.png', width = w, height = h, units='px', res = 300)
plot(LPA_low_extreme_cropped, "gc_id", ext = LPA_low_extreme, col = gc_green, pax = list( 
  labels = F,
  tick = F))
dev.off()

# painted_mountain_gc
LPA_moderate = generate_extent(-12433905, 3956387, fixed_distance)
LPA_moderate_cropped <- crop(gc_30m, LPA_moderate)

png('photos/final_figures/patch_visualizations/LPA_moderate.png', width = w, height = h, units='px', res = 300)
plot(LPA_moderate_cropped, "gc_id", ext = LPA_moderate, col = c("#FFFFFF","#FFFFFF","#FFFFFF", gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()

# luke falcon dunes
LPA_high_extreme = generate_extent(-12511078, 3969232, fixed_distance)
LPA_high_extreme_cropped <- crop(gc_30m, LPA_high_extreme)

png('photos/final_figures/patch_visualizations/LPA_high_extreme.png', width = w, height = h, units='px', res = 300)
plot(LPA_high_extreme_cropped, "gc_id", ext = LPA_high_extreme, col = c(gc_green), pax = list( 
  labels = F,
  tick = F))
dev.off()


#### . ####
#### variability in area ####

# setwd('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures')
# landscape_pattern_path <- "/Users/joshuagilman/Documents/code/PhD/dissertation_chap_3/photos/final_figures"

# all metrics for each course
lpa_to_join2 <- golf_course_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_id" = "class") %>%
  select(gc_id, value) %>%
  group_by(gc_id) %>%
  summarise(LPA = max(value))

gc_names_to_join2 <- gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, gc_name, course, num_holes) %>% 
  unique()

gc_par <- read_csv("data/par_data.csv") %>% 
  select(gc_name, course, par)

#### @total gc area ####
total_gc_area <- golf_course_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_id" = "class") %>% 
  merge(., lpa_to_join2, by = "gc_id") %>% 
  merge(., gc_names_to_join2, by = "gc_id") %>% 
  ggplot(aes(x = 1, y = ca)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.0) + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + ylab("Golf course area \n (all courses)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 20),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_gc_area

#### @total area for 18-hole gcs ####
total_gc_eighteen_area <-  golf_course_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_id" = "class") %>% 
  merge(., lpa_to_join2, by = "gc_id") %>% 
  merge(., gc_names_to_join2, by = "gc_id") %>% 
  filter(num_holes == "18") %>% 
  merge(., gc_par, by = c("gc_name", "course")) %>%
  ggplot(aes(x = 1, y = ca)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.0) + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + ylab("Golf course area \n (18-hole courses)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 20),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_gc_eighteen_area

#### @total area for 18-hole, par 72 gcs ####
total_gc_eighteen_72_area <- golf_course_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_id" = "class") %>% 
  merge(., lpa_to_join2, by = "gc_id") %>% 
  merge(., gc_names_to_join2, by = "gc_id") %>% 
  filter(num_holes == "18") %>% 
  merge(., gc_par, by = c("gc_name", "course")) %>%
  filter(par == "72") %>% 
  ggplot(aes(x = 1, y = ca)) + geom_violin(scale = "count", fill = lm_violin_color, col = lm_violin_color) + geom_boxplot(width=0.1, lwd = 2.0) + scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200)) + ylab("Golf course area \n (18-hole, par 72 courses)") + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 20),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

total_gc_eighteen_72_area

#### @par histogram ####

par_histogram <- golf_course_scale_metrics %>% 
  filter(level == "class") %>% 
  spread(., metric, value) %>%
  select(class, ca, np, lsi, ai) %>%
  rename("gc_id" = "class") %>% 
  merge(., lpa_to_join2, by = "gc_id") %>% 
  merge(., gc_names_to_join2, by = "gc_id") %>% 
  filter(num_holes == "18") %>% 
  merge(., gc_par, by = c("gc_name", "course")) %>%
  ggplot(aes(x = par)) + geom_histogram(fill = lm_violin_color, col = lm_violin_color) + scale_x_continuous(limits = c(50,75), breaks = c(50,55,60,65,70, 75)) + scale_y_continuous(limits = c(0,120), breaks = c(0,20,40,60,80,100,120)) + ylab("Number of Courses") + 
  theme(
    axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 20),
    axis.text.x = element_text(margin = margin(t = 5, r = 5, b = 10, l = 20), size = 16),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size = 20),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 10, l = 10), size = 16),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.background = element_rect(colour = NA),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

par_histogram

(total_gc_area) + (total_gc_eighteen_area) + (total_gc_eighteen_72_area) + (par_histogram) + plot_layout(ncol = 2, nrow = 2) + plot_annotation(tag_levels = "a") & theme(
  plot.tag = element_text(size = 24),
  plot.subtitle = element_text(size = 10))

ggsave(filename = "photos/final_figures/gc_area_variability.png", width = 6, height = 5, scale = 1.7)

#### . ####
#### reported values ####

# TABLE -- golf course patch-level metrics
# mean patch area
gc_patch_landscape_scale_metrics %>% 
  filter(level == "patch") %>% 
  filter(metric == "area") %>% 
  summarise(average_gc_patch_area = mean(value)) %>%
  View()

# patch shape index
gc_patch_landscape_scale_metrics %>% 
  filter(level == "patch") %>% 
  filter(metric == "shape") %>% 
  summarise(average_gc_patch_shape = mean(value)) %>%
  View()

# mean patch core area index
gc_patch_landscape_scale_metrics %>% 
  filter(level == "patch") %>% 
  filter(metric == "cai") %>% 
  summarise(average_gc_patch_cai = mean(value)) %>%
  View()

# TABLE -- individual golf course level
# average golf course area
golf_course_scale_metrics %>% 
  filter(metric == "ca") %>% 
  # summarise(average_gc_area = mean(value)) %>% 
  View()

# average number of golf course patches
golf_course_scale_metrics %>% 
  filter(metric == "np") %>% 
  count(value) %>% 
  # summarise(average_gc_np = mean(value)) %>%
  View()

# average lsi
golf_course_scale_metrics %>% 
  filter(metric == "lsi") %>% 
  # summarise(average_gc_lsi = mean(value)) %>% 
  View()

# average ai
golf_course_scale_metrics %>% 
  filter(metric == "ai") %>% 
  summarise(average_gc_ai = mean(value)) %>% 
  View()

# average lpa
golf_course_scale_metrics %>% 
  filter(level == "patch" & metric == "area") %>% 
  rename("gc_id" = "class") %>%
  select(gc_id, value) %>%
  group_by(gc_id) %>%
  summarise(LPA = max(value)) %>%
  summarise(average_LPA = mean(LPA)) %>% 
  View()

# TABLE -- landscape-level metrics
# all of these belong in a table.
gc_patch_landscape_scale_metrics %>% 
  filter(level == "class") %>% 
  View()

# num 9 hole courses
gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, num_holes) %>%
  distinct() %>% 
  filter(num_holes == 9) %>% 
  View()

# num 18 hole courses
gc_polygons_sf %>% 
  st_drop_geometry() %>% 
  select(gc_id, num_holes) %>%
  distinct() %>% 
  filter(num_holes == 18) %>% 
  View()

# BUFFER values
buffer_df %>% 
  View()

# equity data
# Use rf_func to get the relative frequencies. That is the only way to do it. 

