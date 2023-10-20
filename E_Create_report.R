
# Helper script to run Baleen_whale_analysis_report.Rmd

library(here)
library(knitr)

################################################################################ 

# SET PARAMETERS

# required:

deployment = "JOBW_2019_04" # name of deployment to summarize
metadata = "deployment_summary.csv" # name of metadata csv file
missing_data = FALSE # true if there is missing data within deployment period


# if missing_data = TRUE, specify dates below (if false, this will be ignored):

missing_data_start = as.Date("2020-01-17", format="%Y-%m-%d")
missing_data_end = as.Date("2020-09-10", format="%Y-%m-%d")

################################################################################

# don't need to modify anything below

# define function
render_report = function(
  deployment, metadata, missing_data, missing_data_start, missing_data_end) {
  
  rmarkdown::render(here("R code", "RMarkdown Report",
    "Baleen_whale_analysis_report.Rmd"), 
      params = list(
        deployment = deployment,
        metadata = metadata,
        missing_data = missing_data,
        missing_data_start = missing_data_start,
        missing_data_end = missing_data_end
      ),
    output_file = here("Results", deployment, paste0(deployment, "_analysis_report.docx"))
  )
}

# render report based on parameters set above
render_report(deployment, metadata, missing_data, missing_data_start, missing_data_end)
