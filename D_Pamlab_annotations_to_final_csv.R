# Compile Pamlab annotations into a csv, for multi-species csvs

# G. Macklin 2022 (based on script by P. Emery)


# Download and install packages if not already installed: data.table, tidyverse, here

if (!require("data.table")) install.packages("data.table")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")

# Then open the packages

library(data.table)
library(tidyverse)
library(here)


### EDIT THESE:
file_path <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhaleValidation_InterimResults\Results\SABV_2019_06\annotations)" #copy file path to annotations folder within recording folder, paste inside r"( )"

deployment_code <- "SABV_2019_06" #input deployment code here STN_YYYY_MM


### RUN THESE:

AnalysisCode <- "DPA" #input analysis code here
SpeciesCode <- "MB" # input species code here

file_list <- list.files(file_path, pattern = ".log", full.names = TRUE)


# Then merge them together into a single dataframe

annomerge <- rbindlist(sapply(file_list, fread, simplify = FALSE, USE.NAMES = TRUE, fill=TRUE), fill=TRUE)%>%
  mutate(filename = str_extract(Soundfile, "[^\\\\]*$"), .after=Soundfile)


# Export csv file
output_file = paste0(deployment_code,"_",AnalysisCode,"_",SpeciesCode, "_Annotations_FINAL.csv")
write_csv(annomerge, here("Results", deployment_code, output_file))
