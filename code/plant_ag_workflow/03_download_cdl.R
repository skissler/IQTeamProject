# Optional CDL download step
# install.packages("remotes")
# remotes::install_github("vanichols/cdlTools")
library(cdlTools)
cdl_summary <- getCDLData(aoi = "IA", year = 2022, type = "county", format = "table")
write_csv(cdl_summary, "data/processed/ia_cdl_summary_2022.csv")