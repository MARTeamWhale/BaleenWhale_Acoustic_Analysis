##Author: C. Evers, updated by G. Macklin

# Creates a figure that displays the current analysis status of all deployments for baleen acoustic analysis


# Download and install packages if not already installed: data.table, tidyverse, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(tidyverse, lubridate, ggpattern)

####Set it up####

#read in workplan data
effort_data <-read.csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Codes\DeploymentSummary-Baleenanalysis.csv)"))

#list deployments that are: processed, partially analyzed, fully analyzed

base_dir <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\UNDER CONSTRUCTION - Abandon All Hope, Ye Who Enter Here\Deployments)"

sub_dirs<- list.dirs(base_dir, full.names = TRUE, recursive = TRUE)

sub_dirs_results <-sub_dirs[grepl("Results", sub_dirs)]
sub_dirs_results <-sub_dirs_results[!grepl("annotations", sub_dirs_results)]
sub_dirs_results <-sub_dirs_results[!grepl("analysis_report_files", sub_dirs_results)]


fully_analyzed <- character(0)

for (sub_dir in sub_dirs_results) {
  sub_dir_name <-str_extract(sub_dir, "([A-Za-z]{3,}_\\d{4}_\\d{2})")
  matching_file <- file.path(sub_dir, paste0(sub_dir_name, "_analysis_report.docx"))
  
  if (file.exists(matching_file)) {
    fully_analyzed <- c(fully_analyzed, sub_dir_name)
  }
}

partially_analyzed <- anti_join(tibble(value=str_extract(sub_dirs_results, "([A-Za-z]{3,}_\\d{4}_\\d{2})")), data_frame(value=fully_analyzed), by="value") %>%
  rename(Partial=value)

fully_analyzed<- fully_analyzed %>% 
  as_tibble() %>% 
  rename(Full=value)

sub_dirs_valid <- list.dirs(base_dir_valid, full.names = TRUE, recursive = FALSE)

sub_dirs_valid <-sub_dirs[grepl("Validation", sub_dirs)]
sub_dirs_valid <-sub_dirs_valid[!grepl("ArkLite_Inputs", sub_dirs_valid)]
sub_dirs_valid <-sub_dirs_valid[!grepl("LFDCS_Outputs", sub_dirs_valid)]
sub_dirs_valid <-sub_dirs_valid[!grepl("spectrograms", sub_dirs_valid)]

processed <- anti_join(tibble(value = str_extract(sub_dirs_results, "([A-Za-z]{3,}_\\d{4}_\\d{2})")),tibble(value=str_extract(sub_dirs_results, "([A-Za-z]{3,}_\\d{4}_\\d{2})")), tibble(value=fully_analyzed), by="value") %>%
  rename(Processed=value)


max_rows <- max(nrow(partially_analyzed),nrow(fully_analyzed),nrow(processed))

if (nrow(partially_analyzed) < max_rows) {
  partially_analyzed[(nrow(partially_analyzed) + 1):max_rows, ] <- NA
}

if (nrow(fully_analyzed) < max_rows) {
  fully_analyzed[(nrow(fully_analyzed) + 1):max_rows, ] <- NA
}

if (nrow(processed) < max_rows) {
  processed[(nrow(processed) + 1):max_rows, ] <- NA
}

ALL <- cbind(partially_analyzed,fully_analyzed,processed)

#set it up
effort <- effort_data %>%
  mutate(Start = as_date(as.character(Start), format = "%d-%b-%y"), 
         End = as_date(as.character(End), format="%d-%b-%y")) %>%
  mutate(Deployment = str_replace_all(Deployment, "-", "_")) %>% 
  mutate(Status = ifelse(is.na(Status),
                         case_when(
                           Deployment %in% ALL$Full ~ 1,
                           Deployment %in% ALL$Partial~ 2,
                           Deployment %in% ALL$Processed ~3,
                           TRUE~ 4), 
                         Status)) %>% 
  mutate(Status=as_factor(Status)) %>%
  #mutate(Status= fct_expand(Status, "4"),
  #       Status= fct_relevel(Status, "4", after=3))%>%
  mutate(Site = fct_reorder(Site_Real, Latitude, .desc=TRUE)) # organize sites by latitude


#without taggart lab amar
status_labels <-c("analyzed",   "analysis in progress","processed, awaiting analysis","data downloaded","recovered, awaiting data download","data collection underway", "no data recovered")
status_fill <- c("slateblue4",   "slateblue2",         "skyblue3",                    "grey47",         "grey77",                           "grey87",                   "red4")
status_alpha <- c(0.7,             0.3,                  0.7,                          0.7,               0.5,                               0.3,                          0.7)
status_patt <- c("none",          "none",              "none",                         "none",            "none",                            "none",               "none")
status_size <- c(0,.05,.07,0,.05,.07,0)


# set theme
theme_set(theme_bw())


#### create figure####
p<- ggplot (effort) +
  
  facet_wrap(~Site, ncol=1, strip.position="left") +
  
  geom_rect_pattern(aes(xmin=Start, xmax=End, ymin=0, ymax=1, 
                        fill=Status, alpha=Status, pattern=Status),
                    color = "black", 
                    pattern_fill = "black",
                    pattern_angle = 75,
                    pattern_density = 0.01,
                    pattern_spacing = 0.2,
                    pattern_key_scale_factor = 0.6) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as_date("2015-01-01"), as_date("2023-12-31")),
               expand=c(0,0)) +
  scale_fill_manual(values=status_fill, labels=status_labels, drop = FALSE) +
  scale_alpha_manual(values=status_alpha, labels=status_labels, drop = FALSE) +
  scale_pattern_manual(values=status_patt, labels=status_labels, drop = FALSE) +
  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(size = 0.5, colour = "grey90"),
        legend.key.size = unit(1, 'cm'))+
  theme(strip.text.y.left = element_text(angle = 0))
        
p

ggsave(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\)",
              paste0("PAM_analysis_summary_Baleen_",format(Sys.Date(),"%Y%m%d"),"_GM.png")),p, width = 7, height = 8, dpi = 300)


