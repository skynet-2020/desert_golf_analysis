README 

# Project Title
Data and Rscripts for reproducing the desert golf sustainability paper.

# Introduction
The purpose of this repository is to allow anyone who is interested to reproduce our analysis from our research paper titled, “Re-envisioning Par for the Course: An Assessment of Desert Golf Course Sustainability from a Landscape Perspective”. The repository contains three .R scripts: lm_calculations_dgr.R, spatial_analysis_dgrepo.R, and water_analysis_dgrepo.R. The repository also contains data derived from public resources.

lm_calculations should be ran first. This file cleans the golf course spatial data from the golf course shape file. Then, it calculates landscape metrics for the golf course spatial data at the golf course level, golf club level, and the landscape level. After the file is ran, there will be 4 .rda files that can be used for the golf course spatial and water use analysis. Saving .rda files makes it so the landscape metrics only have to be calculated one time. It takes a long time. 

spatial_analysis_dgrepo.R or water_analysis_dgrepo.R can be ran next. The spatial analysis .R file handles all aspects of our golf course spatial analysis. The water analysis .R file explores the relationship between golf course spatial pattern and water use. The datasets for the analyses are all present in the repository except for two. The missing datasets are not included here because there may be restrictions to sharing these data. Both datasets were obtained as public records request from the Arizona Department of Water Resources. If you need them, I am happy to help you get permissions from ADWR. Without these datasets, water_analysis_dgrepo.R will not run. Without them, lm_calculations_dgr.R and spatial_analysis_dgrepo.R will run.

# Installation
To use the files/data, you can clone this repository onto your own computer. Or you can download the repository as a ZIP file by clicking the ‘download ZIP button’ on the repository page. After cloning or downloading, navigate to the files on your device. Locate a folder on your device where you can put the files. Make sure to keep the directory format the same because the files are set up to run using relative paths. The only thing you should have to change on the files is the working directory. Currently, I have marked the ‘set working directory’ (setwd function) command in all three .R scripts with a TODO. Get the full path and paste it into each .R file in the location I have specified. Then, the files should run on your computer and they should produce the photos/figures used in our paper. Or at least the version of the figures before some of them were edited in keynote. Again, the water_analysis_dgrepo.R will not run without the ADWR public data request data. 

The analysis was conducted using R v4.2.1 and Rstudio v.2022.07.1.

# Contact
My email address is jcgilman@asu.edu. Feel free to contact me if you have any questions about our analysis.