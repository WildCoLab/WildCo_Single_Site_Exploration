### 0. Initialize ####
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)

# Set WD if not working in an R project (you should be!)
#setwd("D:/Mitch/JOFF/3.Data/3.4 Data Analysis/3.4.2 Inputs")

# Set project name
proj_name <- "MK"

### 1. Standardize camera deployments ####

# For projects that use Timelapse images to record camera activity/obtruction
# using the misfire tag, use the following code

# Otherwise, upload your manual record of camera activity, combine with station data,
# and make sure headings match

#### 1.1 Make deployment table from tagged timelapse images ####
# load images.csv output file
all_img <- read_csv("raw_data/images.csv") %>% 
  # select only relevant columns
  select(project_id, station_id, orig_file, exif_timestamp,
         image_trigger, misfire, processed, staff) %>% 
  mutate(date_time = as.POSIXct(exif_timestamp)) %>% 
  arrange(station_id, date_time)

### OPTIONAL: Fix date/time errors 
# Example code, uncomment and udpate to run
# # Fix BLT date-time errors and camera malfuction
# BLT29_time_diff <- as.POSIXct("2023-07-12 17:45:00") - # correct time
#   as.POSIXct(all_img$date_time[all_img$station_id == "BLT29"][1])
# 
# BLT38_time_diff <- as.POSIXct("2023-07-13 11:59:53") - # correct time
#   as.POSIXct(all_img$date_time[all_img$station_id == "BLT38"][1])
# 
# all_img <- all_img %>% 
#   # Remove all BLT43 images with date_time "2024-01-31 07:03:41" - camera malfunction
#   filter(!(station_id == "BLT43" & date_time == "2024-01-31 07:03:41")) %>% 
#   mutate(date_time = case_when(station_id == "BLT29" ~ date_time + BLT29_time_diff,
#                                station_id == "BLT38" ~ date_time + BLT38_time_diff,
#                                # UPDATE with date_time ranges to correct within once new, fixed data is available
#                                .default = date_time))

all_img <- all_img %>% 
  mutate(date = as.Date(date_time),
         station_id_date = paste(station_id, date, sep = "_"))

tl_img <- all_img %>% 
  # filter to timelapse images
  filter(image_trigger == "Time Lapse") %>% 
  arrange(station_id, date_time)

cam_active <- tl_img %>% 
  filter(misfire == F,
         processed == T) %>% 
  # keep only one timelapse per day (in case of mistakes in setting timelapse)
  group_by(station_id, date) %>% 
  slice(1) %>% 
  # check whether there are missing dates in timelapse images
  group_by(station_id) %>% 
  mutate(row_number = row_number(),
         total_rows = n(),
         date_status = case_when(row_number == 1 ~ "activity_start", # first timelapse at station
                                 row_number == total_rows ~ "activity_end", # last timelapse at station
                                 date == lag(date + 1) ~ "consecutive", # continuously active days
                                 date != lag(date + 1) & date != lead(date - 1) ~ "single_active_day",
                                 date != lag(date + 1) ~ "activity_start" # gap in date from last active day
         ),
         # make final active day before a new period the "activity_end"
         date_status = if_else(date_status == "consecutive" & 
                                 lead(date_status) %in% c("activity_start", "single_active_day"),
                               "activity_end", date_status),
         tmp = if_else(date_status == "single_active_day", 2, 1)) %>% 
  # need to duplicate rows for dates when camera activity starts AND ends (only active one day)
  uncount(tmp) %>% 
  # make start and end for single active days
  mutate(date_status = case_when(date_status == "single_active_day" & 
                                   lead(date_status) == "single_active_day" &
                                   date == lead(date) ~ "activity_start",
                                 date_status == "single_active_day" & 
                                   lag(date_status) == "single_active_day" &
                                   date == lag(date) ~ "activity_end",
                                 .default = date_status))

activity_periods <- cam_active %>% 
  select(project_id, station_id, date, date_status, 
         row_number, total_rows) %>% 
  filter(date_status %in% c("activity_start", "activity_end")) %>% 
  # add deployment period number within each station
  group_by(station_id, date_status) %>% 
  mutate(act_period_number = row_number()) 

activity_periods_wide <- activity_periods %>% 
  select(-row_number, -total_rows) %>% 
  pivot_wider(names_from = date_status, values_from = date)

# Load motion images to account for gaps in Timelapse
motion_imgs <- all_img %>% 
  filter(image_trigger != "Time Lapse", # keep motion and "unknown" trigger
         # keep only processed images
         processed == T) %>% 
  arrange(station_id, date_time)

# Get days when timelapse images marked as misfires (=camera view obstructed/camera non-functional)
days_obstructed <- tl_img %>% 
  filter(processed == T,
         misfire == T) %>%
  select(station_id, date, station_id_date) %>% 
  distinct()

# Get all days when images were taken at a station (minus obstructed days)
days_images_taken <- motion_imgs %>%
  group_by(station_id, date) %>% 
  slice(1) %>% 
  # remove days when cams were obstructed
  filter(!(station_id_date %in% days_obstructed$station_id_date))

# check out images that aren't included in activity periods
image_activity_mismatch <- motion_imgs %>%
  filter(!(station_id_date %in% cam_active$station_id_date))

# Adjust activity starts and activity ends based on motion detection images the day before or after
activity_periods <- activity_periods %>% 
  mutate(date = case_when(date_status == "activity_start" & 
                            paste(station_id, date - 1, sep = "_") %in% 
                            days_images_taken$station_id_date ~ date - 1, 
                          date_status == "activity_end" &
                            paste(station_id, date + 1, sep = "_") %in% 
                            days_images_taken$station_id_date ~ date + 1,
                          .default = date))
# QUESTION: Do we always want to include an additional day after activity end if there
# is a motion detection? Or only for the final deployment end (likely staff)?

activity_periods_wide <- activity_periods %>%
  select(-c(row_number, total_rows)) %>%
  pivot_wider(names_from = date_status, values_from = date)

### Stations without any timelapse photos
# use first and last motion images, assume activity between these periods?
# or incorporate camera check data if available? --> not now.
if(length(unique(days_images_taken$station_id)) >
   length(unique(activity_periods$station_id))){
  no_timelapse <- days_images_taken %>% 
    filter(!(station_id %in% activity_periods$station_id)) %>% 
    group_by(project_id, station_id) %>% 
    summarise(activity_start = min(date),
              activity_end = max(date))
  
  # merge with activity periods from timelapse
  activity_periods_wide <- bind_rows(activity_periods_wide,
                                     no_timelapse)
}

##### Stations with periods of missing timelapse
# check for motion images outside of activity windows
# start date of deployments should be the most recent check, or just the first image date

# Check if there are any motion images outside of activity periods
within_tl_activity_periods <- days_images_taken %>% 
  left_join(activity_periods_wide, by = c("station_id", "project_id"),
            relationship = "many-to-many") %>% 
  filter(between(date, activity_start, activity_end))

outside_tl_activity_periods <- days_images_taken %>% 
  filter(!(station_id_date %in% within_tl_activity_periods$station_id_date))

if(nrow(outside_tl_activity_periods) > 0){
  # We need to know whether there was a staff detection at the end of the activity period
  # before the period without timelapse which would confirm that the cam
  # was active at the start of the period. If yes, then make that the start date; 
  # if no, then use the date of the first image
  
  staff_detections <- motion_imgs %>% 
    filter(staff == "True")
  
  # Stations with non-timelapse periods
  # need to split up chunks of detections separated by timelapse activity periods
  non_tl_periods <- outside_tl_activity_periods %>% 
    full_join(activity_periods %>% filter(station_id %in% 
                                            outside_tl_activity_periods$station_id),
              by = c("project_id", "station_id", "date")) %>% 
    arrange(station_id, date) %>% 
    group_by(station_id) %>% 
    # Assign activity period id based on presence of activity starts
    mutate(non_tl_act_period_number = cumsum(!is.na(date_status))) %>% 
    filter(is.na(date_status)) %>% 
    group_by(station_id, non_tl_act_period_number) %>% 
    summarise(first = min(date),
              last = max(date)) %>% 
    left_join(activity_periods_wide,
              relationship = "many-to-many") %>% 
    # find tl_activity end date closest to first motion image
    group_by(station_id, non_tl_act_period_number) %>% 
    mutate(days_since_activity_end = first - activity_end,
           no_tl_activity_before = if_else(all(as.numeric(days_since_activity_end) < 0),
                                           T, F)) %>% 
    filter(no_tl_activity_before == T |
             days_since_activity_end > 0) %>% 
    arrange(station_id, non_tl_act_period_number, days_since_activity_end) %>% 
    slice(1) %>% 
    # were staff detected on the final day of the last activity period (or the day before)
    mutate(staff_detected = case_when(no_tl_activity_before == T ~ F,
                                      paste(station_id, activity_end, sep = "_") 
                                      %in% staff_detections$station_id_date |
                                        paste(station_id, activity_end - 1, sep = "_")
                                      %in% staff_detections$station_id_date ~ T, 
                                      .default = F),
           activity_start = if_else(staff_detected == T, activity_end,
                                    first),
           activity_end = last) %>% 
    ungroup() %>% 
    select(-c(first, last, act_period_number, non_tl_act_period_number,
              days_since_activity_end, staff_detected, no_tl_activity_before))
  
  # merge with other activity periods
  activity_periods_wide <- bind_rows(activity_periods_wide,
                                     non_tl_periods)
}

# plot so that you can take a look for mistakes
days_images_taken_plotting <- motion_imgs %>%
  group_by(station_id, date) %>% 
  slice(1) %>% 
  mutate(obstructed = if_else((station_id_date %in% days_obstructed$station_id_date),
                              "cam obstructed", "cam active"))

ggplot(activity_periods_wide, aes(y = station_id)) +
  geom_segment(aes(x = activity_start, xend = activity_end)) +
  # plot single day deployments separately
  geom_point(data = activity_periods_wide %>% filter(activity_start == activity_end), 
             aes(x = activity_start), shape = 15, size = 0.2) +
  geom_point(data = days_images_taken_plotting, aes(x = date, y = station_id,
                                                    colour = obstructed),
             shape = "|", alpha = 0.7, size = 2) +
  labs(x = "", y = "", colour = "Days with \nmotion images") +
  scale_x_date(date_breaks = '1 month',
               date_labels = '%b %Y') +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### 1.2 combine with other station data ####
stations <- read_csv("raw_data/stations.csv") %>% 
  select(station_id, latitude, longitude)
camera_checks <- read_csv("raw_data/camera_checks.csv") %>% 
  select(-c(check_date, stop_date, battery, camera_label, project_id,
            camera_check_tbl_id, camera_status, media_recovered, images_per_trigger)) %>% 
  # Camera heights, bait status, and other features may change between deployments...
  # average camera heights for now and use a single value for each station
  group_by(station_id) %>% 
  mutate(camera_height = mean(camera_height),
         camera_distance = mean(camera_distance)) %>% 
  slice(1) %>% 
  # replace 0's (default) with NAs for camera height
  mutate(camera_height = if_else(camera_height == 0, NA, camera_height))

deployment <- left_join(activity_periods_wide, camera_checks,
                                         by = "station_id") %>% 
  left_join(stations)
head(deployment)

deployment <- deployment %>%
  rename(
    Deployment.Location.ID = station_id,
    Treatment = treatment,
    Latitude = latitude,
    Longitude = longitude,
    Camera.Deployment.Begin.Date = activity_start,
    Camera.Deployment.End.Date = activity_end,
    Bait.Type = bait,
    Feature.Type = feature,
    Quiet.Period.Setting = quiet_period,
    Height = camera_height,
    Angle = camera_angle,
    d.Trail = camera_distance
  ) %>% 
  arrange(Deployment.Location.ID, Camera.Deployment.Begin.Date)

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
                                d.Trail)

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

# Export to .csv
write.csv(stations, paste0("raw_data/",proj_name,"_station_data.csv"), row.names = F)

### 3. Standardize identifications from database output ####

tags <- read_csv("raw_data/tags.csv")
images <- read_csv("raw_data/images.csv") %>% 
  rename(image_id = `_id`)

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
  filter(Blank == F,
         Species != "No animal")

# Remove duplicate tags (all tagged fields the same except comments)
ud_dat <- ud_dat %>% 
  distinct(Deployment.Location.ID, Date_Time.Captured, Species, 
           Sex, Age, Number.of.Animals,
           Minimum.Group.Size, Behaviour, Collar, collar_type, Collar.ID,
           .keep_all = T) #keep all columns in output

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
