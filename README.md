# Group- and individual-based approaches to health inequality: towards an integration

Supporting information for GROUP- AND INDIVIDUAL-BASED APPROACHES TO HEALTH INEQUALITY: TOWARDS AN INTEGRATION by Iñaki PERMANYER, Isaac SASSON, and Francisco VILLAVICENCIO, published at Journal of the Royal Statistical Society Series A: Statistics in Society 186(2): 217–240 (2023). https://doi.org/10.1093/jrsssa/qnac001.

This repository contains the following files and folders:

1) GiniDecomp.R: R code to reproduce all the results and figures from the paper. Any user should be able to run the code in R or RStudio, with no need to manipulate the file, but installing/uploading all the necessary libraries.

2) Functions.R: File with all the necessary functions to reproduce the results and figures from the paper. It includes functions to calculate the decompositions of the Theil and Gini indices. The file is sourced by GiniDecomp.R and does not need to be executed by the user.

3) The Data folder contains life tables from the United States by sex, race, and ethnicity from 1970 to 2018, and population shares by sex and race from 1970 to 2017. These data are necessary to reproduce Figures 3 to 7 from the main manuscript. The data are automatically uploaded when executing the R-script GiniDecomp.R.
