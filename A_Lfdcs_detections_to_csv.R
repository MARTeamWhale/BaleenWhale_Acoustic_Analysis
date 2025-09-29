## Baleen Detection Table
# converting LFDCS detection output to a table of baleen detections for Pamlab validation

# Authors: G. Macklin and J. Stanistreet 2022


# Download and install packages if not already installed: pacman
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse,lubridate,readxl)


### Change these ####

deployment_code ="XXX_####_##" ###Deployment code here

Folderpath <- r"(PATH TO WAV FOLDER)"   #path to .wav folders on working hard drive

data_source <- ""  # MAR if from any DFO-MAR projects, otherwise code for the folder 

Bm_audible = TRUE # If blue whale audible calls were detected, mark TRUE. If not, mark FALSE

### Run These ####

## Read .wav filenames into R ##

Year <- str_sub(deployment_code, start = -7, end = -4)
Month <- str_sub(deployment_code, start = -2, end = -1)

audio_data <-list.files(Folderpath, pattern = ".wav")

audio <- audio_data %>%
  as_tibble_col(column_name = "Filename") %>%
  mutate(datestring = str_extract(Filename, "\\d{8}\\w\\d{6}\\w")) %>%
  mutate(filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ"))

# select relevant deployment folder

## Read LFDCS outputs and .wav filenames into R for Sei,Humpback, Blue whale audible and Right Whales  ##

lfdcs_fileHF <- list.files(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                  r"(\Validation\LFDCS_Outputs\)"), pattern = "_HF_")

lfdcs_dataHF <- read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                r"(\Validation\LFDCS_Outputs\)", lfdcs_fileHF), header= FALSE)

### PLEASE MAKE SURE THERE ARE 11 VARIABLES! IF THERE ISN'T, RESAVE THE CSV ###

sp_HF_code <- c("1"="Bb","2"="Bb","3"="Bb","5"="Eg",'6'="Eg",'7'="Eg",'8'="Eg",'9'="Eg",
                '15'="Mn",'16'="Mn",'17'="Mn",'18'="Mn",'19'="Mn",'20'="Mn",
                '30'="BmA",'31'="BmA",'32'="BmA",'33'="BmA",'34'="BmA")

lfdcsHF<-lfdcs_dataHF%>%
 {# Detect blank rows before modifying
    blank_indices <- which(apply(., 1, function(row) all(is.na(row) | row == "")))
    start_row <- blank_indices[2] + 1
    cleaned <- slice(., start_row:nrow(.))} %>% 
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

  
## Make the detection sheet for Sei and Humpback ##

detectionsHF<- lfdcsHF%>%
  transmute(Filename=Filename,
            Species = case_when(Species %in% names(sp_HF_code)~ sp_HF_code[Species],
                                TRUE~ NA_character_),
            MD2 = case_when(MDist <=2.00 ~1, TRUE~0),
            MD3 = case_when(MDist <=3.00 ~1, TRUE~0),
            MD4 = case_when(MDist <=4.00 ~1, TRUE~0)) %>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4 = sum(MD4)) %>%
  ungroup()

## Read LFDCS output into R for Blue and Fin ##

lfdcs_fileLF <- list.files(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                  r"(\Validation\LFDCS_Outputs\)"), pattern = "_LF_")

lfdcs_dataLF <- read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                r"(\Validation\LFDCS_Outputs\)", lfdcs_fileLF), header= FALSE)

### PLEASE MAKE SURE THERE ARE 11 VARIABLES! IF THERE ISN'T, RESAVE THE CSV ###

sp_LF_code <- c("1"="Bp","2"="BmT","3"="BmT","4"="BmT")

lfdcsLF<-lfdcs_dataLF%>%
  filter(!row_number() %in% c(1:22))%>%
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

## Make the detection sheet for Blue and Fin ##

detectionsLF<- lfdcsLF%>%
  transmute(Filename=Filename,
            Species = case_when(Species %in% names(sp_LF_code)~ sp_LF_code[Species],
                                TRUE~ NA_character_),
            MD2 = case_when(MDist <=2.00 ~1, TRUE~0),
            MD3 = case_when(MDist <=3.00 ~1, TRUE~0),
            MD4 = case_when(MDist <=4.00 ~1, TRUE~0)) %>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4= sum(MD4)) %>%
  ungroup()

## Make an All Species Detection List ##

detectionsALL <-bind_rows(detectionsLF,detectionsHF)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_MBDetections.csv"))

#### Make filename lists per species, per MDIST for Arklite ###


##Blue Whale
files_BmT3 <- detectionsALL %>%
  filter(Species == "BmT", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmT3.csv"), col_names = FALSE)


files_BmT4 <- detectionsALL %>%
  filter(Species == "BmT", MD4 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmT4.csv"), col_names = FALSE)

files_BmA2 <- detectionsALL %>%
  filter(Species == "BmA", MD2 !=0)%>%
  select(Filename) %>% 
{if (nrow(.)>0) write_csv(.,paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                        r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmA2.csv"), col_names = FALSE)
  else print("Data not available")}
  

files_BmA4 <- detectionsALL %>%
  filter(Species == "BmA", MD4 !=0)%>%
  select(Filename) %>% 
{if(nrow(.)>0) write_csv(.,paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                                           r"(\Validation\ArkLite_Inputs\)",deployment_code,"_BmA4.csv"), col_names = FALSE)
 else print("Data not available")}


## Fin Whale
files_Bp2 <- detectionsALL %>%
  filter(Species == "Bp", MD2 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bp2.csv"), col_names = FALSE)


files_Bp3 <- detectionsALL %>%
  filter(Species == "Bp", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bp3.csv"), col_names = FALSE)

## Sei Whale
files_Bb2 <- detectionsALL %>%
  filter(Species == "Bb", MD2 !=0)%>%
  select(Filename) %>% 
write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                 r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bb2.csv"), col_names = FALSE)

files_Bb4 <- detectionsALL %>%
  filter(Species == "Bb", MD4 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bb4.csv"), col_names = FALSE)

## Humpback Whale
files_Mn2 <- detectionsALL %>%
  filter(Species == "Mn", MD2 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn2.csv"), col_names = FALSE)

files_Mn3 <- detectionsALL %>%
  filter(Species == "Mn", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn3.csv"), col_names = FALSE)

## Right Whale

files_Eg3 <- detectionsALL %>%
  filter(Species == "Eg", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn3.csv"), col_names = FALSE)

files_Eg4 <- detectionsALL %>%
  filter(Species == "Eg", MD4 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn3.csv"), col_names = FALSE)