#' Read Conta Results
#'
#' This function reads the contaminants provided by Skyline and renames the contaminant groups.
#'
#' @param  file_report_skyline The file containing the Skyline report.
#' @param  simplify_ContaminantGroup Flag if contaminant names should be simplified.
#'
#' @return dataframe with the transformed input.
#'
#' @examples
#' read_conta_results(file_report_skyline = PeaksAreasContaminantsFile, simplify_contaminantGroup = TRUE)
#'
#' @export
read_conta_results <- function(file_report_skyline, simplify_ContaminantGroup = TRUE){
  output <-
    read.csv_auto_sep(file_report_skyline, na.strings = c("", "#N/A"))
  #rename columns
  # Skyline generates reports with different column names in function of the steps followed to export the report (e.g., from the Document Grid or from File), and the language (e.g., English or Invariant)
  colnames_f1 <-c("Protein",	"Peptide",	"ReplicateName",	"PeptideRetentionTime",	  "TotalAreaMs1", 	"TicArea") # format when exported with language = Invariant
  colnames_f2 <- c("Protein",	"Peptide",	"Replicate Name",	"Peptide Retention Time",	"Total Area MS1",	"Total Ion Current Area") # format when exported with language = English
  colnames_f3 <-c("Protein",	"Peptide",	"Replicate.Name",	"Peptide.Retention.Time",	"Total.Area.MS1",	"Total.Ion.Current.Area") # format either exported by Skyline or induced by read.csv
  names(output) <-  mgsub::mgsub( names(output), colnames_f1, colnames_f2)
  names(output) <-  mgsub::mgsub( names(output), colnames_f2, colnames_f3)
  # format output
  output <- output %>%
    rename_with(~gsub(patt = "[.]", rep = "",  x = .x)) %>%
    rename(Contaminant = "Peptide",
           ContaminantGroup = "Protein",
           RetentionTime = "PeptideRetentionTime") %>%
    mutate(Contaminant = fct_recode(as.factor(Contaminant),  #Correct names to facilitate ordering
                                    "PEG01" = "PEG1",  "PEG02" = "PEG2", "PEG03" = "PEG3",
                                    "PEG04" = "PEG4", "PEG05" = "PEG5", "PEG06" = "PEG6",
                                    "PEG07" = "PEG7", "PEG08" = "PEG8", "PEG09" = "PEG9",
                                    "PEG01_Na" = "PEG1_Na", "PEG02_Na" = "PEG2_Na", "PEG03_Na" = "PEG3_Na",
                                    "PEG04_Na" = "PEG4_Na", "PEG05_Na" = "PEG5_Na", "PEG06_Na" = "PEG6_Na",
                                    "PEG07_Na" = "PEG7_Na", "PEG08_Na" = "PEG8_Na", "PEG09_Na" = "PEG9_Na",
                                    "PEG01_NH4" = "PEG1_NH4", "PEG02_NH4" = "PEG2_NH4", "PEG03_NH4" = "PEG3_NH4",
                                    "PEG04_NH4" = "PEG4_NH4", "PEG05_NH4" = "PEG5_NH4", "PEG06_NH4" = "PEG6_NH4",
                                    "PEG07_NH4" = "PEG7_NH4", "PEG08_NH4" = "PEG8_NH4", "PEG09_NH4" = "PEG9_NH4",
                                    "PPG01" = "PPG1",  "PPG02" = "PPG2", "PPG03" = "PPG3",
                                    "PPG04" = "PPG4", "PPG05" = "PPG5", "PPG06" = "PPG6",
                                    "PPG07" = "PPG7", "PPG08" = "PPG8", "PPG09" = "PPG9",
                                    "PPG01_Na" = "PPG1_Na", "PPG02_Na" = "PPG2_Na", "PPG03_Na" = "PPG3_Na",
                                    "PPG04_Na" = "PPG4_Na", "PPG05_Na" = "PPG5_Na", "PPG06_Na" = "PPG6_Na",
                                    "PPG07_Na" = "PPG7_Na", "PPG08_Na" = "PPG8_Na", "PPG09_Na" = "PPG9_Na",
                                    "PPG01_NH4" = "PPG1_NH4", "PPG02_NH4" = "PPG2_NH4", "PPG03_NH4" = "PPG3_NH4",
                                    "PPG04_NH4" = "PPG4_NH4", "PPG05_NH4" = "PPG5_NH4", "PPG06_NH4" = "PPG6_NH4",
                                    "PPG07_NH4" = "PPG7_NH4", "PPG08_NH4" = "PPG8_NH4", "PPG09_NH4" = "PPG9_NH4",
                                    "Nylon_C24H44N4O4H" = "C24H44N4O4H", "Nylon_C36H66N6O6H" = "C36H66N6O6H", "Nylon_C48H88N8O8H" = "C48H88N8O8H"),
           Contaminant = as.character(Contaminant), #needed to enable reordering below
           # ContaminantFull = paste(ContaminantGroup, Contaminant, sep = "_"),
           TotalAreaMS1 = replace_na(TotalAreaMS1, rep= 0), #Convert NAs to 0, avoid loosing info
           Abundance = signif(TotalAreaMS1/TotalIonCurrentArea, 4)) %>%
    arrange(Contaminant) %>%
    mutate(across(all_of(c("ContaminantGroup", "Contaminant","ReplicateName")), as.factor)) %>%
    arrange(ReplicateName)

  if(simplify_ContaminantGroup == TRUE){
    # Groups with n <= 3 are simplified
    output <-
      output %>%
      group_by(ContaminantGroup) %>%
      mutate(ContaminantGroup =  as.factor(case_when(n_distinct(Contaminant) <= 3 ~ "Others",
                                                     TRUE ~ as.character(ContaminantGroup)))) %>%
      ungroup()
    message("ContaminantGroups with less than three elements were combined into 'Others'")
  }
  return(output)
}
