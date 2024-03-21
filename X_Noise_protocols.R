#Noise protocols for baleen whale analysis

#Author: G Macklin 2023

# This is a series of steps to be used when the top tier of a species detector has more than 3000 files, which will be first checked by LTSA, removing
#noise heavy files from the tiers before annotating for whale species

#These steps will:
# 1. Move files (listed in the species/tier specific Arklite input) into a separate, temporary LTSA folder
# 2. Convert logs made in the LTSA to annotations to be opened in Pamlab
# 3. Delete the temporary LTSA folder

# Download and install packages if not already installed: tidyverse, lubridate, readxl, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse,lubridate,readxl,here)

options(digits.secs = 0) 

## Change these ##

deployment_code <- "XXX_####_##"

wav_folder <-  r"(PATH TO WAV FILES HERE)" #copy file path to recording folder on working hard drive, paste inside r"( )"

# specify target species:
# BmT	= Blue whale tonal  BmA= Blue whale audible     Bp	= Fin whale     Bb	= Sei whale     Mn	= Humpback whale

sp_name <- "Bm"

#specify the tier of a species to validate:
# Blue whale tonal: 3 or 4  Blue whale audible: 2 or 4   Fin Whale: 2 or 3  Sei Whale: 2 or 4   Humpback Whale: 2 or 3

tier <- "#"

#specify the date of the most updated tier

tier_date <- "YYYY-MM-DD"

sampling_rate = "8000.0" #change to the sampling rate of the deployment

operator = "LASTNAMEFIRSTINITIAL" #Analysts last name and first initial

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### STEP 1: Move files into a separate, temporary LTSA folder ####

ltsa_folder_name <- paste0(sp_name,tier,"_LTSA") #names the folder for files making the LTSA

ltsa_folder <- dir.create(paste0(wav_folder,"\\",ltsa_folder_name)) # creates the folder to put the LTSA files in


#Lists all files in a tier, to be extracted for LTSA
file_list <- read_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",
                             deployment_code, r"(\Validation\Arklite_Inputs\)",
                           paste0(deployment_code, "_", sp_name,tier,"-updated",tier_date ,".csv")), col_names = "filename") %>% 
  mutate(filepath= paste0(wav_folder, "\\", filename)) %>% 
  select(filepath) %>% 
  unlist()

file.copy(file_list,paste0(wav_folder,"\\",ltsa_folder_name)) # copies LTSA files from main directory to LTSA folder

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### STEP 2: Convert LTSA log excel files to annotations ####
ltsa_folder_name <- paste0(sp_name,tier,"_LTSA") #names the folder for files making the LTSA

ltsa_folder <-  paste0(wav_folder,"\\",ltsa_folder_name) #path to LTSA folder

Year <- str_sub(deployment_code, start = -7, end = -4)
Month <- str_sub(deployment_code, start = -2, end = -1)

audio_data <-list.files(ltsa_folder, pattern = ".wav") #lists all .wav files in LTSA folder

#read in audio data
audio <- audio_data %>%
  as_tibble_col(column_name = "Filename") %>% #rename column
  mutate(datestring = str_extract(Filename, "\\d{8}\\w\\d{6}\\w")) %>% #extract datetime string
  mutate(filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ")) # convert to datetime

#get metadata for deployment
metadata <- read_csv(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\PAM_metadata\deployment_summary.csv)") %>% 
  mutate(Deployment= str_replace_all(Deployment,"-", "_")) %>% 
  filter(Deployment == deployment_code)

#read in LTSA logger csv
LTSA_data <-read_excel(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",
                              deployment_code, r"(\Results\)",deployment_code,"_",sp_name,tier,"_LTSA.xls"), sheet=1)

#prep logger csv to convert to annotations  
  LTSA_log <- LTSA_data %>%   
  select("Input file","Event Number", "Species Code", "Call", "Start time", "End time") %>% #select needed columns
    rename_all(~str_replace_all(., "\\s+", "")) %>%  #remove spaces from column names
  mutate(Starttime = as_datetime(Starttime), #convert logger start time to datetime
         Endtime = as_datetime(Endtime), #convert logger end time to datetime
         EventNumber = as_datetime(EventNumber)) %>%  #convert date of logger to datetime
  rowwise() %>% 
  mutate(Filename = audio$Filename[max(which(audio$filedate <= Starttime))], # reads in filename based on annotation Starttime
         datestring = str_extract(Filename, "\\d{8}\\w\\d{6}\\w"),
         filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ")) %>% 
  ungroup()

#create annotation dataframe  
annotation_fill <-LTSA_log %>% 
  mutate("fieldkey:" = "an:",
         "Soundfile" = paste0(wav_folder,"\\",Filename),
         "Channel" = 0,
         "Sampling freq (Hz)" = sampling_rate,
         "Latitude (deg)" = round(metadata$Latitude,4),
         "Longitude (deg)" = round(metadata$Longitude,4),
         "Recorder ID" = str_extract(wav_folder, "[^\\\\]*$"),
         "Recorder depth" = paste0("-",metadata$Depth_m,".0"),
         "Start date and time (UTC)" = format(filedate,'%Y-%m-%d %H:%M:%OS3'),
         "Annotation date and time (local)" = format(EventNumber,'%Y-%m-%d %H:%M:%OS3'),
         "Recorder type" = "AMARS",
         "Deployment" = paste0("bio.scotian.shelf.",Year,"-",Month,".",deployment_code,".",str_extract(wav_folder, "[^\\\\]*$")),
         "Station" = deployment_code,
         "Operator" = operator,
         "Left time (sec)" = format(as.numeric(Starttime-filedate, units="secs"),nsmall=5),
         "Right time (sec)" = format(as.numeric(Endtime-filedate, units="secs"),nsmall=5),
         "Top freq (Hz)" = "50.00",
         "Bottom freq (Hz)" = "40.00",
         "Species" = case_when((Call =="Vessel Noise"|Call == "Seismic"|Call == "Sonar"|Call == "Mooring"|Call=="Other")~"NN",
                               (Call=="Too Much Noise"|Call=="Some Noise"|Call=="Quiet File")~"NC"),
         "Call type" = case_when(Call =="Vessel Noise"~"HS",
                                 Call == "Seismic"~"S",
                                 Call == "Sonar"~"SO",
                                 Call == "Mooring"~"M",
                                 Call=="Too Much Noise"~"TN",
                                 Call=="Some Noise"~"SN",
                                 Call=="Quiet File"~"QF",
                                 TRUE~ NA),
         "rms SPL" = 999.99999,
         "SEL" = 999.99999,
         "x" = "Help Not Requested",
         "z" = "",
         "y" = case_when((Call =="Vessel Noise"|Call == "Seismic"|Call == "Sonar")~"MM",
                         (Call == "Mooring"|Call == "Other")~"O",
                         (Call=="Too Much Noise"|Call=="Some Noise")~"NS",
                         Call=="Quiet File"~"QT"),
         "xx"="")%>% 
  select(!c(Inputfile:Endtime,datestring,filedate)) %>% 
  mutate(Filename = paste0(Filename,"_chan0 annotations"))

names(annotation_fill)[colnames(annotation_fill)== "x"]<-"" #remove column name for "Help Not Requested"
names(annotation_fill)[colnames(annotation_fill)== "y"]<-"" #remove column name for Something?
names(annotation_fill)[colnames(annotation_fill)== "z"]<-"" #remove column name for "Call Category"
names(annotation_fill)[colnames(annotation_fill)== "xx"]<-"" #remove column name for Something?

anno_split<-split(annotation_fill, f=annotation_fill$Filename) %>%  #split the annotation by filename
  lapply(function(x) { x["Filename"] <- NULL; x })

#write individual annoatations
sapply(names(anno_split), 
         function (x) write.table(anno_split[[x]], file=paste0(wav_folder,"\\annotations\\",x, ".log"),sep = "\t", row.names = FALSE, quote = FALSE))
