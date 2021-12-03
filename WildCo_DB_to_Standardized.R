### 0. Initialize ####
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)

# Set WD if not working in an R project (you should be!)
setwd("D:/Mitch/JOFF/3.Data/3.4 Data Analysis/3.4.2 Inputs")

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
# Select only first occurence, since we're pulling from all cam checks
treatment <- treatment %>%
  group_by(Deployment.Location.ID) %>%
  filter(Deployment.Location.ID == min(Deployment.Location.ID)) %>%
  slice(1) %>%
  ungroup()

stations <- left_join(stations, treatment, by = "Deployment.Location.ID")
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

names(ud_dat)[names(ud_dat)=="project_id"] <- "Project.ID"
names(ud_dat)[names(ud_dat)=="station_id"] <- "Deployment.Location.ID"
names(ud_dat)[names(ud_dat)=="orig_file"] <- "Image.ID"
names(ud_dat)[names(ud_dat)=="misfire"] <- "Blank"
names(ud_dat)[names(ud_dat)=="latin_name"] <- "Species"
names(ud_dat)[names(ud_dat)=="common_names"] <- "Species.Common.Name"
names(ud_dat)[names(ud_dat)=="age_category"] <- "Age"
names(ud_dat)[names(ud_dat)=="exif_timestamp"] <- "Date_Time.Captured"
names(ud_dat)[names(ud_dat)=="sex"] <- "Sex"
names(ud_dat)[names(ud_dat)=="behaviour"] <- "Behaviour"
names(ud_dat)[names(ud_dat)=="group_count"] <- "Minimum.Group.Size"
names(ud_dat)[names(ud_dat)=="species_count"] <- "Number.of.Animals"
names(ud_dat)[names(ud_dat)=="collar"] <- "Collar"
names(ud_dat)[names(ud_dat)=="collar_tags"] <- "Collar.ID"

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
                 comments
)

# Change date and write to csv
write.csv(ud_dat, paste0("raw_data/",proj_name,"_detection_data.csv"), row.names = F)
