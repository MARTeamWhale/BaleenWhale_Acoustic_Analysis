library(pacman)
p_load(tidyverse,lubridate,readxl)

## Change these ##

#audio folder
wav_folder <- r"(D:\GLSW_2022_10\ST6436.192000.d24)"

deployment_code <- "GLSW_2022_10"

analyst_initial <- "GM"

## Run these ##

input <- read_excel(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",
                           deployment_code,"/Validation/",deployment_code,"_BIO_",analyst_initial,'_matlab.xlsx'), sheet=1)

pamlab_check <- input %>% 
  filter(Class_MATLAB != "Incorrect") %>% 
  select(FileName) %>% 
  distinct() %>% 
  write_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                   r"(\Validation\ArkLite_Inputs\)",deployment_code,"_pamlabcheck.csv"), col_names = FALSE)


data<- input %>%
  select(!c(Class_LFDCS,ReasonForUNK,Comments)) %>% 
  filter(Class_MATLAB != "Incorrect") %>% 
  mutate(datestring = str_extract(FileName, "\\d{8}\\w\\d{6}\\w")) %>%
  mutate(filedate = as_datetime(datestring, format="%Y%m%dT%H%M%SZ")) %>% 
  mutate(SigEnd = ifelse(SigStart==SigEnd,SigEnd+1, SigEnd))


NARWdetections <- tibble(`fieldkey:`= "an:",
                      Soundfile = paste0(wav_folder,"/",data$FileName),
                      filename= paste0(data$FileName,"_chan0 annotations"),
                      Channel = 0,
                      `Sampling freq (Hz)`= '8000.0',
                      `Latitude (deg)`='0.0',
                      `Longitude (deg)`= '0.0',
                      `Recorder ID`= str_extract(wav_folder, "AMAR\\d+"),
                      `Recorder depth` = '-0.0',
                      `Start date and time (UTC)` = format(data$filedate,'%Y-%m-%d %H:%M:%OS3'),
                      `Annotation date and time (local)` = format(as_datetime(Sys.time()),'%Y-%m-%d %H:%M:%OS3'),
                      `Recorder type` = 'AMAR',
                      Deployment = deployment_code,
                      Station = str_extract(deployment_code, "^[^_]+"),
                      Operator = 'GMacklin',
                      `Left time (sec)` = data$SigStart,
                      `Right time (sec)` = data$SigEnd,
                      `Top freq (Hz)`= 300,
                      `Bottom freq (Hz)`= 50,
                      Species= "RW",
                      `Call type` = "U",
                      `rms SPL` = 100,
                      SEL =  100,
                      V23 =  'Help Not Requested',
                      V24 =  "",
                      V25 = "UP",
                      V26 =  "",
                      V27 =  "")

names(NARWdetections)[colnames(NARWdetections)== "V23"]<-"" #remove column name for "Help Not Requested"
names(NARWdetections)[colnames(NARWdetections)== "V25"]<-"" #remove column name for "Call Category"
names(NARWdetections)[colnames(NARWdetections)== "V26"]<-"" #remove column name for "Awesomeness"
names(NARWdetections)[colnames(NARWdetections)== "V27"]<-"" #remove column name for Something?
names(NARWdetections)[colnames(NARWdetections)== "V24"]<-"" #remove column name for Something?

NARWdetections[is.na(NARWdetections)] <- ""


anno_split<-split(NARWdetections, f=NARWdetections$filename) %>%  #split the annotation by filename
  lapply(function(x) { x["filename"] <- NULL; x })

#write individual annoatations
dir.create(file.path(wav_folder, "annotations"))

sapply(names(anno_split), 
       function (x) write.table(anno_split[[x]], file=paste0(wav_folder,"/annotations/",x, ".log"),sep = "\t", row.names = FALSE, quote = FALSE))

### DON"T FORGET TO DELETE ANNOTATIONS WHEN COMPLETE!###