library(HowDirty)
getwd()
setwd("example")

get_annotation_template(file_report_skyline = file.path("data",
                                                        "PeakAreas_Contaminants_2021-231.csv"))
get_report_template(file = "test_report_20260511")
