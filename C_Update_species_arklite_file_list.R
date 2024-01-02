
# J. Stanistreet October 2022

# Script to create a new list of files for validation based on original list of files with detections, after:

#   1) removing full days which already contain 1 or more annotations of the target species
#   2) removing individual files which have already been reviewed in PamLab, based on existing annotations of all species

# output csv is formatted as a simple file list for ArkLite

########
# Download and install packages if not already installed: data.table, tidyverse, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse, lubridate)


######## INPUT (modify these lines) ########

# specify deployment name:
deployment_code <- "MGL_2018_09"

# specify the date on the most CURRENT annotations .csv file:
annotation_date <- "2023-09-28"

# specify target species:
# BmT	= Blue whale tonal  BmA	= Blue whale audible   Bp	= Fin whale     Bb	= Sei whale     Mn	= Humpback whale

sp_name <- "Bm"

# specify species code used in annotations:
# BW	= Blue whale     FW	= Fin whale     SW	= Sei whale     HB	= Humpback whale

sp_code <- "BW"

#Specify the call category/type targeted by detector
# IF = Blue whale tonals, # A = Blue whale audible  IS = Fin whale pulses  FF = Sei whale downsweeps   NS","SG = Humpback whale (all call types)

sp_call <- c("IF")

#specify the tier of a species to validate:
# Blue whale: 3 or 4   Fin Whale: 2 or 3  Sei Whale: 2 or 4   Humpback Whale: 2 or 3

tier <- "4"


######## PROCESS (no need to modify the lines below) ########

# 1) load files
annotation_csv <- paste0(deployment_code,"_DPA_MB_Annotations_",annotation_date,".csv")

annotations <- read_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                               r"(\Results\interim_csvs\)", annotation_csv))

species_detections <- read_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                                      r"(\Validation\Arklite_Inputs\)", paste0(deployment_code, "_", sp_name,tier, ".csv")), col_names = "filename") # very important to specify col_names here

# 2) add filename and filedate variables to annotation table
annotations <- annotations %>% 
  mutate(filename = str_extract(Soundfile, "[^\\\\]*$")) %>% 
  mutate(filedate = as_date(`Start date and time (UTC)`))

# 3) get days where target species and call type has already been annotated
annotated_days <- annotations %>% 
  filter(Species == sp_code & V25 %in% sp_call)

# 4) remove all files from days that already contain at least one annotation of the target species and call type from the list of target files
target_detections <- species_detections %>% 
  mutate(datestring = str_extract(filename, "\\d{8}\\w\\d{6}\\w")) %>% 
  mutate(filedate = as_date(datestring)) %>% 
  anti_join(annotated_days, by = "filedate") %>% 

# 5) remove files already viewed from list of target files
      # NOTE: this uses the full list of rows in the annotation table,
      # assuming that any file for which ANY annotation exists has been fully reviewed
  
  anti_join(annotations, by = "filename")


# 6) export simple csv file with new list of target files for ArkLite

#format file list for export
file_list <- target_detections %>% 
  select(filename)

#specify output file name (using current date)
today<-Sys.Date()
output_file<-paste0(deployment_code, "_", sp_name,tier, "-updated", today, ".csv")

#export as csv, removing column header
write_csv(file_list, paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                            r"(\Validation\Arklite_Inputs\)", output_file), col_names = FALSE)
