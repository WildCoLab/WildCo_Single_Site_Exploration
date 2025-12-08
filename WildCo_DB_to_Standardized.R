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

tags <- read.csv("raw_data/tags.csv")
images <- read.csv("raw_data/images.csv") %>% 
  rename(image_id = X_id)

# add image-level data for to tags
ud_dat <- left_join(tags, images, by = "image_id")

# rename to standardized columns
names(ud_dat)

ud_dat <- ud_dat %>%
  rename(Project.ID = project_id,
         Deployment.Location.ID = station_id,
         Image.ID = orig_file,
         Blank = misfire,
         Species = species, # now combined latin and common name column, separate next
         Age = age_category,
         Date_Time.Captured = exif_timestamp,
         Sex = sex,
         Behaviour = behaviour,
         Minimum.Group.Size = group_count,
         Number.of.Animals = species_count,
         Collar = collar,
         Collar.ID = collar_tags,
         Comments = comments.x)

# Separate latin and common species names
unique(ud_dat$Species)
ud_dat <- ud_dat %>% 
  mutate(# Extract text within brackets:
         Species.Common.Name = stringr::str_extract(pattern = "(?<=\\().*?(?=\\))", 
                                                    string = Species),
         # Remove brackets and everthing within them
         Species = gsub(pattern = " \\(.*?\\)", 
                        replacement = "", 
                        x = Species),
         # for species without a common name (e.g. Bird spp.), make species name the common name
         Species.Common.Name = if_else(is.na(Species.Common.Name), 
                                       Species, Species.Common.Name))

# remove blank images and misfires
ud_dat <- ud_dat %>% 
  filter(Blank == "False",
         Species != "No animal")

# small standardizations
ud_dat$Project.ID <- proj_name 
ud_dat$Time.Zone <- NA
ud_dat$Time.Zone <- "UTC-8" # Change if in different TZ

# pare down to desired columns only
ud_dat <- ud_dat %>% 
  transmute(Project.ID,
            Deployment.Location.ID,
            Image.ID,
            Blank = is.logical(Blank),
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
            Comments)

# Write to csv
write.csv(ud_dat, paste0("raw_data/",proj_name,"_detection_data.csv"), row.names = F)
