### 0. Initialize ####
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)

# Set WD if not working in an R project (you should be!)
#setwd("D:/Mitch/JOFF/3.Data/3.4 Data Analysis/3.4.2 Inputs")

# Set project name
proj_name <- "JOFF"

### 1. Standardize camera deployments ####
stations <- read_csv("raw_data/stations.csv")
camera_checks <- read_csv("raw_data/camera_checks.csv")

deployment  <- full_join(camera_checks, stations, by = "station_id")
head(deployment)

deployment <- deployment %>%
  rename(
    Deployment.Location.ID = station_id,
    Treatment = treatment,
    Latitude = latitude,
    Longitude = longitude,
    Camera.Deployment.Begin.Date = check_date,
    Camera.Deployment.End.Date = stop_date,
    Bait.Type = bait,
    Feature.Type = feature,
    Quiet.Period.Setting = quiet_period,
    Height = camera_height,
    Angle = camera_angle,
    d.Trail = camera_distance,
    Status = camera_status
  )

deployment_short <- select(deployment, 
                                Deployment.Location.ID,
                                Treatment, 
                                Latitude,
                                Longitude, 
                                Camera.Deployment.Begin.Date,
                                Camera.Deployment.End.Date, 
                                Bait.Type,
                                Feature.Type,
                                Quiet.Period.Setting,
                                Height, 
                                Angle,
                                d.Trail,
                                Status)

write.csv(deployment_short, paste0("raw_data/",proj_name,"_deployment_data.csv"), row.names = F)

### 2. Standardize station data ####
stations <- stations %>% 
  rename(Deployment.Location.ID = station_id,
         Latitude = latitude,
         Longitude = longitude)

# Add treatment column from deployments
treatment <- select(deployment, Deployment.Location.ID, Treatment)
# Select only first occurrence, since we're pulling from all cam checks
treatment <- treatment %>%
  group_by(Deployment.Location.ID) %>%
  filter(Deployment.Location.ID == min(Deployment.Location.ID)) %>%
  slice(1) %>%
  ungroup()

stations <- left_join(stations, treatment, by = "Deployment.Location.ID")

# Trim down
stations <- select(stations, -station_tbl_id, -project_id)

# Export to .csv
write.csv(stations, paste0("raw_data/",proj_name,"_station_data.csv"), row.names = F)

### 3. Standardize identifications from database output ####

idents <- read.csv("raw_data/images_idents.csv")
ud_dat <- idents

# remove deleted rows
ud_dat <- filter(ud_dat, deleted == "f")

# rename to standardized columns
colnames(ud_dat)

ud_dat <- ud_dat %>%
  rename(Project.ID = project_id,
         Deployment.Location.ID = station_id,
         Image.ID = orig_file,
         Blank = misfire,
         Species = latin_name,
         Species.Common.Name = common_names,
         Age = age_category,
         Date_Time.Captured = exif_timestamp,
         Sex = sex,
         Behaviour = behaviour,
         Minimum.Group.Size = group_count,
         Number.of.Animals = species_count,
         Collar = collar,
         Collar.ID = collar_tags)

# remove blank images and misfires
ud_dat$Blank[ud_dat$Blank=="f"] <- F
ud_dat <- filter(ud_dat, Blank == F)
ud_dat <- filter(ud_dat, Species != "No animal")

# remove extraneous species column
ud_dat <- subset(ud_dat, select = -c(species))

# small standardizations
ud_dat$`Project.ID` <- proj_name 
ud_dat$Time.Zone <- NA
ud_dat$Time.Zone <- "UTC-8" # Change if in different TZ

# pare down to desired columns only
ud_dat <- select(ud_dat,
                 Project.ID,
                 Deployment.Location.ID,
                 Image.ID,
                 Blank,
                 Species,
                 Species.Common.Name,
                 Date_Time.Captured,
                 Time.Zone,
                 Number.of.Animals,
                 Minimum.Group.Size,
                 Age,
                 Sex,
                 Behaviour,
                 Collar,
                 Collar.ID,
                 comments)

# Write to csv
write.csv(ud_dat, paste0("raw_data/",proj_name,"_detection_data.csv"), row.names = F)
