# Compile Pamlab annotations into a csv, for multi-species csvs

# G. Macklin 2022 (based on script by P. Emery)


# Download and install packages if not already installed: data.table, tidyverse, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(data.table,tidyverse)


### EDIT THESE:
file_path <- r"()" #copy file path to annotations folder within recording folder, paste inside r"( )"

deployment_code <- "XXXX_####_##" #input deployment code here STN_YYYY_MM

data_source <- ""  # MAR if from any DFO-MAR projects, otherwise code for the folder 


### RUN THESE:

AnalysisCode <- "DPA" #input analysis code here
SpeciesCode <- "MB" # input species code here

file_list <- list.files(file_path, pattern = ".log", full.names = TRUE)


# Then merge them together into a single dataframe

annomerge <- rbindlist(sapply(file_list, fread, simplify = FALSE, USE.NAMES = TRUE, fill=TRUE), fill=TRUE)%>%
  mutate(filename = str_extract(Soundfile, "[^\\\\]*$"), .after=Soundfile) %>% 
  mutate(Deployment= deployment_code,
         Station= str_extract(Deployment, "^[^_]+"))


# Export csv file
output_file = paste0(deployment_code,"_",AnalysisCode,"_",SpeciesCode, "_Annotations_FINAL.csv")
write_csv(annomerge, paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",data_source,"\\",deployment_code,
                            r"(\Results\)", output_file))
