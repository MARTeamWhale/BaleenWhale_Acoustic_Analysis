#compile all baleen whale results together

library(pacman)
p_load(tidyverse, lubridate,readxl)

# CHANGE THESE ----

deployment_code <- "STN_YYYY_MM" # input deployment code

#Specify if Minke log exists
include.minke <- TRUE

# RUN THESE ----

folder_path <-  paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                       r"(\Results\)")

## Bring in metadata ----
metadata.in<- read_csv(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\PAM_metadata\deployment_summary.csv)")

metadata <- metadata.in %>% 
  mutate(Deployment = str_replace_all(Deployment,"-","_")) %>% 
  filter(Deployment==deployment_code) %>% 
  transmute(Deployment, start = as_date(`In-water_start`)+1, 
            end = as_date(`In-water_end`)-1)


##Import all results----

narw.in <- list.files(folder_path, pattern = "matlab.xlsx", full.names = TRUE) %>% 
  read_excel(sheet=1)

big4.in <- list.files(folder_path, pattern = "DPA_MB_Annotations_FINAL.csv", full.names = TRUE) %>% 
  read_csv()

if(include.minke == TRUE){
  
  minke.in <- list.files(folder_path, pattern = "Ba_LTSA.xls", full.names = TRUE) %>% 
    read_excel(sheet=1)

}

## Remove incorrects, non-animal detections, format----

##NARW----
narw <-narw.in %>% 
  filter(Class_MATLAB != "Incorrect") %>% 
  mutate(detecdate = as_date(`SigStartDateTime`),
         species = "Eg") %>% 
  mutate(definite= case_when(Class_MATLAB== "Correct"~1,
                             TRUE~0),
         possible=case_when(Class_MATLAB=="Unknown"~1,
                            TRUE~0)) %>%
  group_by(detecdate,species) %>%
  summarise(definite= ifelse(1 %in% definite, 1,0),
            possible = ifelse(1 %in% possible, 1,0)) %>%
  ungroup() %>% 
  mutate(presence = case_when(definite>0 ~ 'D',
                              definite==0 & possible>0 ~ 'P',
                              definite==0 & possible==0 ~ 'N')) %>% 
  select(!c(definite, possible)) %>% 
  mutate(callcat= "UP", 
         calltype = "U", .after=species)

## Big4----

big4 <- big4.in %>% 
  select(`Start date and time (UTC)`, Species, `Call type`, V25) %>%
  filter(grepl(paste0("^(UN\\|)?(?:",paste(c('BW',"FW","SW","HB", "MW","RW"), collapse = "|"),")$"),Species)) %>% 
  mutate(detecdate=as_date(`Start date and time (UTC)`),
         calltype = `Call type`,
         callcat = V25) %>% 
  mutate(definite= ifelse(!grepl("^(UN\\|)", Species),1,0),
         possible= ifelse(grepl("^(UN\\|)", Species),1,0)) %>% 
  mutate(species = case_when((Species=="BW"|Species=="UN|BW")~ "Bm",
                             (Species=="FW"|Species=="UN|FW")~ "Bp",
                             (Species=="SW"|Species=="UN|SW")~ "Bb",
                             (Species=="HB"|Species=="UN|HB")~ "Mn",
                             (Species=="RW"|Species=="UN|RW")~ "Eg",
                             (Species=="MW"|Species=="UN|MW")~ "Ba")) %>% 
  group_by(detecdate, species,callcat, calltype) %>% 
  summarise(definite= ifelse(1 %in% definite, 1,0),
            possible = ifelse(1 %in% possible, 1,0)) %>%
  ungroup() %>% 
  mutate(presence = case_when(definite>0 ~ 'D',
                              definite==0 & possible>0 ~ 'P',
                              definite==0 & possible==0 ~ 'N')) %>% 
  select(!c(definite, possible))

## Minke ----
if (include.minke = TRUE){
  
minke <-minke.in %>%
  select(`Species Code`,`Start time`,Call) %>% 
  filter(`Species Code` == "Ba") %>% 
  mutate(detecdate = as_date(`Start time`)) %>% 
  mutate(calltype= case_when(str_detect(Call, "Slow-Down")~ "SD",
                             str_detect(Call, "Speed-Up")~ "SU",
                             str_detect(Call, "Constant")~"CT"),
         callcat= "PT") %>% 
  mutate(definite= case_when(Call != "Unspecified"~1,
                             TRUE~0),
         possible=case_when(Call =="Unspecified"~1,
                            TRUE~0)) %>%
  group_by(detecdate,callcat,calltype) %>%
  summarise(definite= ifelse(1 %in% definite, 1,0),
            possible = ifelse(1 %in% possible, 1,0)) %>%
  ungroup() %>% 
  mutate(species="Ba", .after= detecdate)%>% 
  mutate(presence = case_when(definite>0 ~ 'D',
                              definite==0 & possible>0 ~ 'P',
                              definite==0 & possible==0 ~ 'N')) %>% 
  select(!c(definite, possible))

}

# Compile all together ----

 if (include.minke=TRUE){
baleen.daily <- rbind(big4,narw,minke) %>% 
  group_by_all() %>% 
  summarise() %>% 
  ungroup %>% 
  arrange(detecdate,species)
} else {
baleen.daily <- rbind(big4,narw) %>% 
    group_by_all() %>% 
    summarise() %>% 
    ungroup %>% 
    arrange(detecdate,species)
}

# remove days not included in effort (based on full 24-hour recordings)
baleen.daily<-baleen.daily %>% 
  filter(detecdate>=metadata$start) %>% 
  filter(detecdate<=metadata$end)

## write to csv ----
write_csv(baleen.daily, paste0(folder_path,deployment_code,"_baleenwhale_dailypresence.csv"))
