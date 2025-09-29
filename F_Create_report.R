
# Helper script to run Baleen_whale_analysis_report.Rmd

# load packages
library(pacman)
pacman::p_load(here, knitr)

################################################################################ 

# SET PARAMETERS

# required:

deployment = "MBK_2021_09" # name of deployment to summarize
metadata = "deployment_summary.csv" # name of metadata csv file - do not change unless necessary for special case
Bm_audible = FALSE # set as true if blue whale audible detector was used, false if annotations were opportunistic
missing_data = FALSE # set as true if there is missing data within deployment period

# if missing_data = TRUE, specify start and end date(s) of missing data period(s) (if false, this will be ignored)

# NOTES:
# 1) use format c("YYYY-MM-DD", "YYYY-MM-DD")
# 2) if missing data spans two years, list as two separate periods (ending Dec 31 and starting Jan 1)

missing_data_starts = c(" ")
missing_data_ends = c(" ")

################################################################################

# don't need to modify anything below

# define function
render_report = function(
  deployment, metadata, Bm_audible, missing_data, missing_data_starts, missing_data_ends) {
  
  rmarkdown::render(here("RMarkdown_Report",
    "Baleen_whale_analysis_report.Rmd"), 
      params = list(
        deployment = deployment,
        metadata = metadata,
        Bm_audible = Bm_audible,
        missing_data = missing_data,
        missing_data_starts = missing_data_starts,
        missing_data_ends = missing_data_ends
      ),
    output_file = paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\MAR\)",
                         deployment,
                         r"(\Results\)", 
                         paste0(deployment, "_analysis_report_", Sys.Date(), ".docx"))
  )
}

# render report based on parameters set above
render_report(deployment, metadata, Bm_audible, missing_data, missing_data_starts, missing_data_ends)
