# Compile Pamlab annotations into a csv, for multi-species csvs

# G. Macklin 2022 (based on script by P. Emery)


# Download and install packages if not already installed: data.table, tidyverse, here
if (!require("pacman")) install.packages("pacman")

# Then open the packages
library(pacman)
p_load(data.table, tidyverse)

### EDIT THESE:
file_path <- r"(F:\MGL_2018_09\AMAR194.1.8000.M36-V35-100\annotations)" #copy file path to annotations folder within recording folder, paste inside r"( )"

deployment_code <- "MGL_2018_09" #input deployment code here STN_YYYY_MM


### RUN THESE:

AnalysisCode <- "DPA" #input analysis code here
SpeciesCode <- "MB" # input species code here

file_list <- list.files(file_path, pattern = ".log", full.names = TRUE)


# Then merge them together into a single dataframe

annomerge <- rbindlist(sapply(file_list, fread, simplify = FALSE, USE.NAMES = TRUE, fill=TRUE), fill= TRUE)%>%
  mutate(filename = str_extract(Soundfile, "[^\\\\]*$"), .after=Soundfile)


# Export csv file
output_file = paste0(deployment_code,"_",AnalysisCode,"_",SpeciesCode, "_Annotations_",Sys.Date(),".csv")
write_csv(annomerge, paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\)",deployment_code,
                            r"(\Results\interim_csvs)",output_file))
