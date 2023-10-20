# Formats baleen annotations for PACM

# Author: G. Macklin (based on script by J. Stanistreet)

# Install packages

if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")

# Open Packages

library(tidyverse)
library(lubridate)
library(here)

#### Edit these ####

# specify deployment name:
deployment_code <- "FCH_2019_10"

# specify the date on the most CURRENT annotations .csv file:

sampling_rate = 8000 #CHANGE THIS TO THE SAMPLING RATE#

analysis_start <- as_date("2019-10-09") # first full recording day

analysis_end <- as_date("2020-09-01") # last full recording day

### RUN IF THERE IS MISSING DATA

# is there a period of missing data?

missing_data = FALSE

# if missing_data=TRUE, specify missing period

missing_period <-seq.Date(as_date("FIRST DATE OF MISSING DATA YYYY-MM-DD"),as_date("LAST DATE OF MISSING DATA YYYY-MM-DD"), by="day")


### Run these ####

Sys.setenv(TZ = "UTC")

annotation_date <- "FINAL"

annotation_csv <- paste0(deployment_code,"_DPA_MB_Annotations_",annotation_date,".csv")

csvdata <- read_csv(here("Results", deployment_code, annotation_csv), na=c("","NA"))

annodata<-csvdata %>% 
  transmute(filetime = ymd_hms(as.character(`Start date and time (UTC)`)), # parse date times
            species = Species, callcat = V25) %>% # rename columns of interest, drop others
  mutate(filedate = as_date(as.character(filetime)), format = '%Y%m%d %H%M%S')%>% # create column with date only
  group_by(filedate, species, callcat) %>% # this bins by day
  summarise(n_detects =n()) %>%  # output dataset retains one row per day per species/callcat/calltype combo, as well as a count of each output
  ungroup() # removes (unseen) grouping information from dataset

annodata <- annodata %>%
  filter(species == "BW" & callcat %in% c("A","IF") |
           species == "FW" & callcat %in% c("IS") |
           species == "SW" & callcat %in% c("FF", "AL") |
           species == "HB" & callcat %in% c("NS", "TC", "O", "NT") | 
           species == "MW" & callcat == "AL" |
           species == "UN|BW" | species == "UN|FW" | species == "UN|SW" | species == "UN|HB" | species == "UN|MW") %>%
  mutate(callcat = case_when(species=='BW' & (callcat=='A'| callcat=='IF') ~ 'ALL',
                             species=='FW' & (callcat=='IS') ~ 'ALL',
                             species=='SW' & (callcat=='FF'| callcat=='AL') ~ 'ALL',
                             species=='HB' & (callcat=='NS'| callcat=='TC'| callcat== 'O'| callcat == "NT") ~ 'ALL',
                             species=='MW' & (callcat=='AL') ~ 'ALL')) %>%
  group_by(filedate, species) %>%
  summarise(n_detects = sum(n_detects)) %>%
  ungroup()

#create the Unique ID code for the dataset
unique_ID <- paste0(deployment_code,"_LF", sep="")

# species analyzed

species_list <-   c( 'BW', 'UN|BW', 'FW',  'UN|FW','SW', 'UN|SW','HB','UN|HB',  'MW', 'UN|MW') # Pamlab codes for species included in analysis
species_labels <- c('BLWH', 'BLWH','FIWH', 'FIWH','SEWH','SEWH','HUWH','HUWH','MIWH','MIWH') # PACM codes for species included in analysis

# metadata for PACM results

analysis_period = 86400 # NOTE: we are summarizing all detection data per day for submission to PACM
time_zone = 'UTC'
detection_method = 'LFDCS'
protocol_ref = 'DFO 2022'
software = 'IDL'
software_version = '8.5'
min_freq = '0'
max_freq = sampling_rate/2
qc_proc = 'Archival'

# PACM Formatting

detections <- annodata

names(detections) = sub(".*_","",names(detections)) # remove the "Presence_" from column header to leave only species code 

names(species_list) <-species_labels # label species with PACM codes

output <- detections %>% 
  mutate(date_only = as_date(filedate, format= "%Y%m%d"))%>% # add column with date (no time)
  filter(date_only >= analysis_start & date_only <= analysis_end)%>% # filter by analysis dates (full days) to remove data before or after
  group_by(date_only, species, detects, .drop = F)%>% # group by day and species, don't drop missing species levels
  mutate(possible_count = case_when(str_detect(species,"UN") ~1, TRUE ~0),
         true_count = case_when(str_detect(species,"UN", negate=TRUE) ~1, TRUE ~0))%>%
  mutate(species = fct_expand(species, species_list))%>% # add factor levels for species not present in data
  mutate(species = fct_recode(species, !!!species_list))%>%  # rename factor levels with PACM species codes
  summarize(true_count = sum(true_count), possible_count = sum(possible_count)) %>% # this expands out any missing species
  ungroup() %>%
  
  # fill in rows for days with no detections, add values of 0 to the count columns
  complete(date_only = seq.Date(analysis_start,analysis_end, by="day"), nesting(species), fill=list(true_count=0, possible_count=0)) %>% 
  
  # add column with call presence specification using PACM codes
  mutate(call_presence = case_when(true_count>0 ~ 'D',
                                   true_count==0 & possible_count>0 ~ 'P',
                                   true_count==0 & possible_count==0 ~ 'N'))%>%
  # add call_types for each species
  mutate(call_type = case_when(species =="BLWH" ~ "BLMIX",
                               species =="FIWH" ~ "FWPLS",
                               species=="SEWH" ~ "SWDS",
                               species=="HUWH" ~ "HWMIX",
                               species=="MIWH" ~ "MWPT")) %>%
  
  ## format for PACM template 
  mutate(UNIQUE_ID = unique_ID,
         ANALYSIS_PERIOD_START_DATETIME = format_ISO8601(as.POSIXct(date_only, tz="UTC"), usetz=TRUE),
         ANALYSIS_PERIOD_END_DATETIME = format_ISO8601(as.POSIXct((date_only+hours(24)), tz="UTC"), usetz=TRUE),
         ANALYSIS_PERIOD_EFFORT_SECONDS = analysis_period,
         ANALYSIS_TIME_ZONE = time_zone)%>% 
  
  rename(SPECIES = species,
         ACOUSTIC_PRESENCE = call_presence,
         CALL_TYPE = call_type) %>%
  
  mutate(N_VALIDATED_DETECTIONS = case_when(ACOUSTIC_PRESENCE=="D"|ACOUSTIC_PRESENCE=="P" ~detects),
         N_VALIDATED_DETECTIONS = replace_na(N_VALIDATED_DETECTIONS, 0),
         DETECTION_METHOD = detection_method,
         PROTOCOL_REFERENCE = protocol_ref,
         DETECTION_SOFTWARE_NAME = software,
         DETECTION_SOFTWARE_VERSION = software_version, 
         MIN_ANALYSIS_FREQUENCY_RANGE_HZ = min_freq,
         MAX_ANALYSIS_FREQUENCY_RANGE_HZ = max_freq,
         ANALYSIS_SAMPLING_RATE_HZ = sampling_rate,
         QC_PROCESSING = qc_proc) %>% 
  
  # remove extra variables
  select(-c(true_count, possible_count,date_only,detects))%>% 
  
  # put columns in correct order for output csv
  relocate(UNIQUE_ID, ANALYSIS_PERIOD_START_DATETIME, ANALYSIS_PERIOD_END_DATETIME, ANALYSIS_PERIOD_EFFORT_SECONDS, ANALYSIS_TIME_ZONE,
           SPECIES, ACOUSTIC_PRESENCE, N_VALIDATED_DETECTIONS, CALL_TYPE, DETECTION_METHOD, PROTOCOL_REFERENCE, DETECTION_SOFTWARE_NAME,
           DETECTION_SOFTWARE_VERSION, MIN_ANALYSIS_FREQUENCY_RANGE_HZ, MAX_ANALYSIS_FREQUENCY_RANGE_HZ, ANALYSIS_SAMPLING_RATE_HZ,
           QC_PROCESSING)

#Save output

today<-format(Sys.Date(), format="%Y%m%d")
output_csv_file<- str_c("DFOCA_DETECTIONDATA_",today,"_",unique_ID,".csv")
write_csv(output, here("Results",deployment_code,output_csv_file)) 
