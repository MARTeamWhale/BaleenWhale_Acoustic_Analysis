
# Helper script to run Baleen_whale_analysis_report.Rmd
library(here)
library(knitr)

################################################################################ 

# SET PARAMETERS

# required:

deployment = "JOBW_2019_04" # name of deployment to summarize
metadata = "deployment_summary.csv" # name of metadata csv file
missing_data = FALSE # true if there is missing data within deployment period

# if missing_data = TRUE, specify start and end date(s) of missing data period(s) (if false, this will be ignored)

# NOTES:
# 1) use format c("YYYY-MM-DD", "YYYY-MM-DD")
# 2) if missing data spans two years, list as two separate periods (ending Dec 31 and starting Jan 1)

missing_data_starts = c("2020-11-16","2021-03-09","2021-05-05")
missing_data_ends = c("2020-12-24","2021-03-28","2021-05-24")

################################################################################

# don't need to modify anything below

# define function
render_report = function(
  deployment, metadata, missing_data, missing_data_start, missing_data_end) {
  
  rmarkdown::render(here("RMarkdown Report",
    "Baleen_whale_analysis_report.Rmd"), 
      params = list(
        deployment = deployment,
        metadata = metadata,
        missing_data = missing_data,
        missing_data_start = missing_data_start,
        missing_data_end = missing_data_end
      ),
    output_file = paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenAcousticAnalysis\Deployments\)",deployment,
                         r"(\Results\)", paste0(deployment, "_analysis_report.docx"))
  )
}

# render report based on parameters set above
render_report(deployment, metadata, missing_data, missing_data_start, missing_data_end)
