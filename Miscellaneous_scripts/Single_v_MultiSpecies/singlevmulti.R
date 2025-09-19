library(pacman)

p_load(tidyverse, purrr, ggpattern)

#CHANGE THESE ----

# species/calltype of interest
# Blue whale infrasonic: BmT, Blue whale audible: BmA, Fin whale: Bp,
# Sei whale": Bb, Humpback whale: Mn, Right whale: Eg

single_sp = "Bp"


# RUN THESE ----

# Set the path to your main folder
main_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments)" # direct to all deployments

# List all subfolders
subfolders <- list.files(main_folder, full.names = TRUE)#list all subfolders (aka Validation, Results)
subfolders <-subfolders[!str_detect(subfolders, "TRAIN")]

## Get all results ----

# Initialize an empty list to store dataframes
dfs <- list()

# Iterate over each subfolder, read the CSV files, and combine into a single dataframe
for (subfolder in subfolders) {
  # Extract fruit name from the subfolder name
  deployment <- basename(subfolder) #extract deployment name
  
  # Read the CSV file
  csv_file <- list.files(paste0(subfolders,"\\Results\\"), pattern = paste0(deployment, "_FINAL.csv"), #find the presence csv
                         full.names = TRUE)
  
  df <- read_csv(csv_file) #read presence csv
  
  df$deployment <- deployment
  
  df<- df %>% 
    mutate(deployment = deployment,
           station = str_extract(deployment, "^[^_]+")) %>%
    relocate(station,deployment)
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
  dfs <- keep(dfs, ~ nrow(.) > 0)
  
}

dfs <- lapply(dfs, function(x) {
  select(x,
         station,
         deployment,
         filename,
         species = Species,
         call_type = `Call type`,
         call_cat = V25)
})

data.input <- bind_rows(dfs)

sp_list <- c("BW","FW","SW","HB","RW")

all.results <- data.input %>% 
  filter(species %in% sp_list) %>% 
  mutate(sp_code = case_when((species == "BW" & call_cat =="A")~ "BmA",
                             (species == "BW" & call_cat =="IF") ~ "BmT",
                             species == "FW" & call_cat =="IS"~ 'Bp',
                             species == "SW" & call_cat =="FF"~ "Bb",
                             species == "HB"~ 'Mn',
                             species == "RW" ~ 'Eg')) %>% 
  filter(!is.na(sp_code))


## Get all detections ----

# Initialize an empty list to store dataframes
dfs <- list()

# Iterate over each subfolder, read the CSV files, and combine into a single dataframe
for (subfolder in subfolders) {
  # Extract fruit name from the subfolder name
  deployment <- basename(subfolder) #extract deployment name
  
  # Read the CSV file
  csv_file <- list.files(paste0(subfolders,"\\Validation\\Arklite_Inputs\\"), pattern = paste0(deployment,"_MBDetections.csv"), #find the detection csv
                         full.names = TRUE)
  
  df <- read_csv(csv_file) #read presence csv
  
  df$deployment <- deployment
  
  df<- df %>% 
    mutate(deployment = deployment,
           station = str_extract(deployment, "^[^_]+")) %>%
    relocate(station,deployment)
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
  dfs <- keep(dfs, ~ nrow(.) > 0)
  
}

detection.input <- bind_rows(dfs) %>% select(-`...3`)

## Isolate detections of interest ----

tier_list <- c(BmT="MD4",BmA="MD4",Bp="MD3",Bb="MD4",Mn="MD3", Eg="MD4")

detections <- detection.input %>%
  filter(deployment %in% unique(all.results$deployment)) %>% 
  filter(Species == single_sp) %>% 
  filter(.data[[tier_list[[single_sp]]]] != 0) %>% 
  select(station,deployment,'filename'=Filename,'sp_code'=Species) %>% 
  mutate(detection = 1) %>% 
  unique() 

## Isolate results of interest ----

results <- all.results %>% 
  filter(sp_code == single_sp) %>% 
  mutate(presence = 1) %>% 
  select(station,deployment,filename, sp_code,presence) %>% 
  unique() 

day.results <- full_join(results,detections) %>% 
  mutate(presence = replace_na(presence,0),
         detection = replace_na(detection,0)) %>% 
  arrange(station,deployment,filename) %>% 
  
  mutate(datestring = str_extract(filename, "\\d{8}\\w\\d{6}\\w")) %>% 
  mutate(filedate = as_date(datestring, format="%Y%m%dT%H%M%SZ")) %>%
  
  group_by(station,deployment,sp_code,filedate) %>% 
  summarise(presence = sum(presence),
            detection = sum(detection)) %>% 
  ungroup()

fig.results <- day.results %>% 
 group_by(station,deployment,sp_code) %>% 
  summarise(days_detect = sum(presence >0 & detection > 0),
            days_nodetect = sum(presence>0 & detection ==0)) %>% 
  mutate(percent = (days_detect/(days_detect+days_nodetect))*100) %>% 
  
  
  mutate(year = str_extract(deployment, "\\d{4}")) %>% 
  
  pivot_longer(cols = c(days_detect, days_nodetect),
               names_to = "detection",
               values_to = "days") %>% 
  mutate(detection = recode(as.character(detection),
                        days_detect = "Presence, Detected by Species Detector",
                        days_nodetect = "Presence, Not Detected by Species Detector")) %>% 
  mutate(detection = factor(detection, levels = c("Presence, Not Detected by Species Detector", "Presence, Detected by Species Detector")))



## Graph that ----

years = unique(fig.results$year)

cols <-c( "#1F78B4","#A6CEE3","#7570B3", "#F0E442", "#E7298A", "#D55E00")
names(cols) <- names(tier_list)

sp_label <-c("Blue whale (tonal calls)","Blue whale (audible calls)","Fin whale","Sei whale","Humpback whale", "North Atlantic right whale")
names(sp_label) <- names(tier_list)

for (i in years) {

year.fig.results <- fig.results %>%filter(year == as.numeric(i))

p <-ggplot()+

  geom_bar_pattern(data = year.fig.results, aes(x = station, y = days, fill = sp_code, pattern = detection, pattern_fill=sp_code), 
                   stat= 'identity', position="stack",
  pattern_fill = "black",         # Color of the stripe lines
  pattern_angle = 45,
  pattern_density = 0.1,
  pattern_spacing = 0.01,
  pattern_key_scale_factor = 0.6)+
  
  scale_pattern_manual(values = c("Presence, Detected by Species Detector" = "none", "Presence, Not Detected by Species Detector" = "stripe")) +
  scale_fill_manual(values = cols) +
  scale_pattern_fill_manual(values = cols) +
  
  geom_text(data = year.fig.results %>% group_by(station,sp_code,percent) %>% summarise(days=sum(days)) %>% ungroup(), aes(x = station, y= days+2, label = paste0(round(percent), "%"))) +
    
  labs(title = paste0(i," Deployments: ", sp_label[unique(year.fig.results$sp_code)]),
       x = "Station",
       y= "Days with Presence") +
  
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y= element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill= "white", colour = "grey50"),
        axis.text = element_text(size = 12), 
     #  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        panel.border = element_rect(fill = NA, colour = "black"),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm")) +
    
    guides(fill = "none",pattern = guide_legend(
        title = "Detection",
        override.aes = list(fill = unname(cols[single_sp]),         # Pick a color to show in legend
          pattern_fill = unname(cols[single_sp]), # Match fill to background
          pattern_colour = "black")))
print(p)

plotname <- paste0(i,"_",single_sp,"_singlevmulti.png")

ggsave(here(Miscellaneous_scripts, Single_v_MultiSpecies,output_figures,plotname),p, width = 9.5, height =  4.5, units = "in" )
}

