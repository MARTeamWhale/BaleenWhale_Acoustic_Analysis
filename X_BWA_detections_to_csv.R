## Baleen Detection Table
# converting LFDCS detection output to a table of baleen detections for Pamlab validation

# Authors: G. Macklin and J. Stanistreet 2022


# Download and install packages if not already installed: pacman
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse,lubridate,readxl)


### Change these ####

deployment_code =""
analyst1 = "" ## HF analyst initial names (from lfdcs output filename)

Folderpath <- r"()"   #path to .wav folders on working hard drive

### Run These ####

## Read .wav filenames into R ##

Year <- str_sub(deployment_code, start = -7, end = -4)
Month <- str_sub(deployment_code, start = -2, end = -1)

audio_data <-list.files(Folderpath, pattern = ".wav")

audio <- audio_data %>%
  as_tibble_col(column_name = "Filename") %>%
  mutate(datestring = str_extract(Filename, "\\d{8}\\w\\d{6}\\w")) %>%
  mutate(filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ"))

## Read LFDCS outputs and .wav filenames into R for Blue whale audible calls  ##

lfdcs_fileBWA <- paste0(deployment_code,"_BWA","_BIO_",analyst1,".csv")

lfdcs_dataBWA <- read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                                r"(\Validation\LFDCS_Outputs\)", lfdcs_fileBWA), header= FALSE)

### PLEASE MAKE SURE THERE ARE 11 VARIABLES! IF THERE ISN'T, RESAVE THE CSV ###

sp_HF_code <- c('30'="BmA",'31'="BmA",'32'="BmA",'33'="BmA",'34'="BmA")

lfdcsBWA<-lfdcs_dataBWA%>%
  filter(row_number() > 22) %>% 
  select(c(V1,V2,V9))%>%
  rename(Species=V1,
         StartTime = V2,
         MDist=V9)%>%
  mutate(StartTime = as.numeric(StartTime),
         StartDateTime = as_datetime(StartTime))%>%
  rowwise() %>% 
  mutate(Filename = audio$Filename[max(which(audio$filedate <= StartDateTime))]) %>% 
  ungroup()%>%
  select(-c(StartTime, StartDateTime))


## Make the detection sheet for blue whale audible calls ##

detectionsBWA<- lfdcsBWA%>%
  transmute(Filename=Filename,
            Species = case_when(Species %in% names(sp_HF_code)~ sp_HF_code[Species],
                                TRUE~ NA_character_),
            MD2 = case_when(MDist <=2.00 ~1, TRUE~0),
            MD3 = case_when(MDist <=3.00 ~1, TRUE~0),
            MD4 = case_when(MDist <=4.00 ~1, TRUE~0)) %>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4 = sum(MD4)) %>%
  ungroup()

all.detect <-  read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                               r"(\Validation\ArkLite_Inputs\)",deployment_code,"_MBDetections.csv"))

detectionsALL <-bind_rows(detectionsBWA,all.detect)%>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4 = sum(MD4)) %>%
  ungroup() %>% 
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_MBDetections.csv"))

#### Make filename lists per species, per MDIST for Arklite ###


##Blue Whale

files_BmA2 <- detectionsBWA %>%
  filter(Species == "BmA", MD2 !=0)%>%
  select(Filename) %>% 
  {if (nrow(.)>0) write_csv(.,paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                                     r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmA2.csv"), col_names = FALSE)
    else print("Data not available")}


files_BmA4 <- detectionsBWA %>%
  filter(Species == "BmA", MD4 !=0)%>%
  select(Filename) %>% 
  {if(nrow(.)>0) write_csv(.,paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                                    r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmA4.csv"), col_names = FALSE)
    else print("Data not available")}
