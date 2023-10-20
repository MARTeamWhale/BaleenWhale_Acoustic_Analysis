# Compile Pamlab annotations into a csv, for single species csvs

# G. Macklin 2022 (based on script by P. Emery)

if (!require("data.table")) install.packages("data.table")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")

# Then open the packages

library(data.table)
library(tidyverse)
library(here)


### EDIT THESE:
file_path <- r"()" #copy file path to annotations folder within recording folder, paste inside r"( )"

deployment_code <- "WRITE DEPLOYMENT CODE HERE" #input deployment code here STN_YYYY_MM

# Bm	= Blue whale               Bp	= Fin whale                 Bb	= Sei whale              Ba	= Minke whale   
# Mn	= Humpback whale           Eg	= Right whale               NN	= Non-biological noise   NC	= No Baleen Calls

SpeciesCode <- "Bm" # input species code here


### RUN THESE:

AnalysisCode <- "DPA" #input analysis code here

file_list <- list.files(file_path, pattern = ".log")


# Then merge them together into a single dataframe

annomerge <- rbindlist(sapply(file_list, fread, simplify = FALSE, USE.NAMES = TRUE, fill=TRUE)) %>%
  mutate(filename = str_sub(Soundfile, -30), .after=Soundfile)


species <- subset(annomerge, annomerge$Species== SpeciesCode)

species_file <- paste0(Deployment_code,"_",AnalysisCode,"_",SpeciesCode, "_Annotations.csv")

write.csv(species, here("Results", Deployment_code), species_file)