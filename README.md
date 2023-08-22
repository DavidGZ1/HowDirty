# HowDirty

## About

HowDirty is an R package that assesses the level of contamination of LC-MS results.
The presence of contaminants (e.g., PEG), and detergents (e.g., CHAPS, SDS) in samples analyzed by LC-MS can be severely detrimental to identifying peptides/proteins or other molecules. Skyline is used to extra MS1 features of many known contaminant masses from raw files (e.g., .raw and .d). The results are exported to a .csv file, then processed in R using HowDirty to generate an HTML interactive report that evaluates sample contamination risks. For more details, please see our [preprint](https://www.authorea.com/users/643346/articles/656759-howdirty-an-r-package-to-evaluate-molecular-contaminants-in-lc-ms-experiments) and the tutorial (below).

## Tutorial
Detailed instructions can be found in the [tutorial](https://github.com/DavidGZ1/HowDirty/blob/main/tutorial/HowDirty_tutorial.pdf).

## Requirements

-	Raw LC-MS results to be evaluated
-	Skyline version > 4 [1,2].To install it, you can register online and download the latest version here: https://skyline.ms/project/home/software/Skyline/begin.view
-	Skyline HowDirty [template](https://github.com/DavidGZ1/HowDirty/tree/main/tutorial), including the Skyline molecular contaminant transition list [3] and reports configuration
-	Alternatively, you can set up Skyline yourself (further instructions in the [Skyline tutorials](https://skyline.ms/wiki/home/software/Skyline/page.view?name=tutorials))
    -	Download the molecular contaminant transition list [3] from [Panorama](https://panoramaweb.org/project/Panorama%20Public/2018/Amgen%20-%20Molecular%20Contaminants/begin.view?) and load it into Skyline:File / Import / Transition List…
    -	Create the PeakAreas_Contaminants report: containing the columns: 
    -	Settings / Document Settings / Report / Add, then add a name and select the columns: "Protein", "Peptide", "Replicate Name", "Peptide Retention Time", "Total Area MS1", "Total Ion Current Area"
    -	Enable the report form by ticking the box next to its name, then click OK
-  R software for data analysis and the R packages Rmarkdown, knitr, and HowDirty

## Installation

```r
install.packages("devtools")
library(devtools)
install_github("kassambara/ggpubr")
install_github("DavidGZ1/HowDirty", force = TRUE)
```

## Usage

See tutorial for detailed instructions on how to extract MS1 features from Skyline and on pipeline usage.

```r
#  Sets the working directory to the desired folder (e.g., where the PeakAreas_Contaminants.csv is stored)
setwd(“C:/Users/Name/ExampleHowDirty”)

# Loads HowDirty package
library(HowDirty)

# Creates sample annotation or experiment design file
get_annotation_template(file_report_skyline  = "PeakAreas_Contaminants.csv")

# Create HowDirty template with the name "example.Rmd"
HowDirty::get_report_template(file = "example")

# Fill the parameters in the header and “knit” (compile) the report. There are two options for this (see screenshots below).
# Using RStudio:
#    a) Click on "Knit with parameters"
#    b) Fill out the parameters values manually, then click "Knit"
#       The parameters are in the header of the .Rmd document after "params:"
#       The values are entered after "value: "
#       Do not modify the text after "label: " or "input: "
```
Knit buttons in RStudio:

![Button options in RStudio for knitting a markdown file. Shots the Knit button with a dropdown menu, that includes 'Knit to HTML', 'Knit to PDF', 'Knit to Word', 'Knit with Parameters'.](https://github.com/DavidGZ1/HowDirty/assets/134387857/be25535d-6583-4b75-8f64-09a407d1d5cf)

a)

![Screenshot of the window popping up when using 'Knit with Parameters'. It shows several text fields with descriptions.](https://github.com/DavidGZ1/HowDirty/assets/134387857/407eba9a-fe0a-47d4-99e6-21874aa47943)

b)

![Screenshot of the parameters section of the .Rmd file, showing where the file names for the PeakAreasContaminantsFile, the AnnotationFile, and (if applicable) the RefThresholdsFile need to be added with examples.](https://github.com/DavidGZ1/HowDirty/assets/134387857/dff65428-d7d0-4e12-9039-49941954cafd)


## References

[1] L.K. Pino, B.C. Searle, J.G. Bollinger, B. Nunn, B. MacLean, M.J. MacCoss, The Skyline ecosystem: Informatics for quantitative mass spectrometry proteomics, Mass Spectrom. Rev. 39 (2020) 229–244. [https://doi.org/10.1002/mas.21540].

[2] B. MacLean, D.M. Tomazela, N. Shulman, M. Chambers, G.L. Finney, B. Frewen, R. Kern, D.L. Tabb, D.C. Liebler, M.J. MacCoss, Skyline: an open source document editor for creating and analyzing targeted proteomics experiments, Bioinformatics. 26 (2010) 966–968. [https://doi.org/10.1093/bioinformatics/btq054].

[3] M.J. Rardin, Rapid Assessment of Contaminants and Interferences in Mass Spectrometry Data Using Skyline, J. Am. Soc. Mass Spectrom. 29 (2018) 1327–1330. [https://doi.org/10.1007/s13361-018-1940-z].


## License

[GPL-3.0](https://github.com/DavidGZ1/HowDirty/blob/main/LICENSE)
