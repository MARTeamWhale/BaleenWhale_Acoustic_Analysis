## Baleen Detection Table
# converting LFDCS detection output to a table of baleen detections for Pamlab validation

# Authors: G. Macklin and J. Stanistreet 2022


# Download and install packages if not already installed: tidyverse, lubridate, readxl, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse,lubridate,readxl)


### Change these ####

deployment_code ="STN_YYYY_MM" ###Deployment code here

analyst1 = "GM" ## HF analyst initial names (from lfdcs output filename)

analyst2 = "GM" ## LF analyst initial names (from lfdcs output filename)

Folderpath <- r"(F:\MGL_2018_09\AMAR194.1.8000.M36-V35-100)"   #path to .wav folders on working hard drive


### Run These ####

## Read .wav filenames into R ##

Year <- str_sub(deployment_code, start = -7, end = -4)
Month <- str_sub(deployment_code, start = -2, end = -1)

audio_data <-list.files(Folderpath, pattern = ".wav")

audio <- audio_data %>%
  as_tibble_col(column_name = "Filename") %>%
  mutate(datestring = str_extract(Filename, "\\d{8}\\w\\d{6}\\w")) %>%
  mutate(filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ"))

## Read LFDCS outputs and .wav filenames into R for Sei and Humpback ##

lfdcs_fileHF <- paste0(deployment_code,"_HF","_BIO_",analyst1,".csv")

lfdcs_dataHF <- read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                                r"(\Validation\LFDCS_Outputs\)", lfdcs_fileHF), header= FALSE)
### PLEASE MAKE SURE THERE ARE 11 VARIABLES! IF THERE ISN'T, RESAVE THE CSV ###

lfdcsHF<-lfdcs_dataHF%>%
  filter(!row_number() %in% c(1:27))%>%
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
            Species = case_when((Species == 1|Species == 2|Species == 3) ~ "Bb",
                            (Species == 15|Species == 16|Species == 17|Species == 18|Species == 19|Species == 20) ~ "Mn"),
            MD2 = case_when(MDist <=2.00 ~1, TRUE~0),
            MD3 = case_when(MDist <=3.00 ~1, TRUE~0),
            MD4 = case_when(MDist <=4.00 ~1, TRUE~0)) %>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4 = sum(MD4)) %>%
  ungroup()

## Read LFDCS output into R for Blue and Fin ##

lfdcs_fileLF <- paste0(deployment_code,"_LF","_BIO_",analyst2,".csv")

lfdcs_dataLF <- read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                                r"(\Validation\LFDCS_Outputs\)", lfdcs_fileLF), header= FALSE)

### PLEASE MAKE SURE THERE ARE 11 VARIABLES! IF THERE ISN'T, RESAVE THE CSV ###

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
            Species = case_when((Species == 2|Species == 3|Species == 4) ~ "Bm",
                                (Species == 1) ~ "Bp"),
            MD2 = case_when(MDist <=2.00 ~1, TRUE~0),
            MD3 = case_when(MDist <=3.00 ~1, TRUE~0),
            MD4 = case_when(MDist <=4.00 ~1, TRUE~0)) %>%
  group_by(Filename,Species) %>%
  summarize(MD2 = sum(MD2), MD3 = sum(MD3), MD4= sum(MD4)) %>%
  ungroup()

## Make an All Species Detection List ##

detectionsALL <-bind_rows(detectionsLF,detectionsHF)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_MBDetections.csv"))

#### Make filename lists per species, per MDIST for Arklite ###


##Blue Whale
files_Bm3 <- detectionsALL %>%
  filter(Species == "Bm", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bm3.csv"), col_names = FALSE)


files_Bm4 <- detectionsALL %>%
  filter(Species == "Bm", MD4 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bm4.csv"), col_names = FALSE)

## Fin Whale
files_Bp2 <- detectionsALL %>%
  filter(Species == "Bp", MD2 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bp2.csv"), col_names = FALSE)


files_Bp3 <- detectionsALL %>%
  filter(Species == "Bp", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bp3.csv"), col_names = FALSE)

## Sei Whale
files_Bb2 <- detectionsALL %>%
  filter(Species == "Bb", MD2 !=0)%>%
  select(Filename) %>% 
write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                 r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bb2.csv"), col_names = FALSE)

files_Bb4 <- detectionsALL %>%
  filter(Species == "Bb", MD4 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Bb4.csv"), col_names = FALSE)

## Humpback Whale
files_Mn2 <- detectionsALL %>%
  filter(Species == "Mn", MD2 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn2.csv"), col_names = FALSE)

files_Mn3 <- detectionsALL %>%
  filter(Species == "Mn", MD3 !=0)%>%
  select(Filename)%>%
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_Mn3.csv"), col_names = FALSE)
