# Script to convert from WildCo database "images_idents" detection output to 
# standardized format
# Created by Mitch Fennell
# Last edited: Nov 26, 2020

library(dplyr)

# set project name
proj_name <- "Cathedral"

# Set your working directory (or work in an R project, like you should be doing)
setwd("D:/Mitch/Cathedral/3. Data/3.4 Data Analysis/3.4.4 Standardized WildCo Analysis")

# read in detection data
og_dat <- read.csv("images_idents.csv", header = T)
head(og_dat)

ud_dat <- og_dat

# remove "deleted" detections
ud_dat <- filter(ud_dat, deleted == "f")

# fix misfire column
ud_dat$misfire[ud_dat$misfire=="f"] <- FALSE
ud_dat$misfire[ud_dat$misfire=="t"] <- TRUE

# Change column names of essential columns to standardized format
colnames(ud_dat)

names(ud_dat)[names(ud_dat)=="project_id"] <- "Project.ID"
names(ud_dat)[names(ud_dat)=="station_id"] <- "Deployment.Location.ID"
names(ud_dat)[names(ud_dat)=="orig_file"] <- "Image.ID"
names(ud_dat)[names(ud_dat)=="misfire"] <- "Blank"
names(ud_dat)[names(ud_dat)=="staff_name"] <- "Photo.Type.Identified.By"
names(ud_dat)[names(ud_dat)=="latin_name"] <- "Species"
names(ud_dat)[names(ud_dat)=="common_names"] <- "Species.Common.Name"
names(ud_dat)[names(ud_dat)=="age_category"] <- "Age"
names(ud_dat)[names(ud_dat)=="exif_timestamp"] <- "Date_Time.Captured"
names(ud_dat)[names(ud_dat)=="sex"] <- "Sex"
names(ud_dat)[names(ud_dat)=="species_count"] <- "Number.of.Animals"
names(ud_dat)[names(ud_dat)=="behaviour"] <- "Behaviour"
names(ud_dat)[names(ud_dat)=="group_count"] <- "Minimum.Group.Size"

# Remove empty extra "species" column
ud_dat <- subset(ud_dat, select = -c(species))
# Add project ID column
ud_dat$`Project ID` <- proj_name

# Save as updated .CSV
write.csv(ud_dat, paste0("Detection_Data_", proj_name,".csv")) 
