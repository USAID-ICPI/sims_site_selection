## SIMS Prioritization tool

[![Build Status](https://travis-ci.org/USAID-OHA-SI/asSIMSble.svg?branch=master)](https://travis-ci.org/USAID-OHA-SI/asSIMSble)

This function provides the underlying data for the SIMS Prioritizatation Tool. It uses the PEPFAR MER Structured Dataset to score sites a number of different metrics developed by the SIMS ST3 in mid-2018.

Creating a new tool for a country is relatively straight forward. 

First you will need to download the [SIMS Prioritization Tool Template](https://github.com/USAID-OHA-SI/asSIMSble/raw/master/Template/sims_prioritization_template.xlsx), which can be found on this repo.

Next, you will need to have R/RStudio installed on your local machine. Through R, you can install the package to create the dataset and then load the data into the Excel template. Using the code below, you will install `devtools` and use that to install this [package from GitHub](https://github.com/USAID-OHA-SI/asSIMSble).

``` {r}
#install devtools
  install.packages("devtools")
  
#install/update package
  devtools::install_github("USAID-OHA-SI/asSIMSble")
#load package
  library("asSIMSble")
```

With the package load, you will need to provide a few things: (1) the file path to the *site* MSD for a given operating unit (downloaded from [PEPFAR Panorama](www.pepfar-panorama.org/)), (2) the file path for the Excel template (downloaded from [GitHub](https://github.com/USAID-OHA-SI/asSIMSble/raw/master/Template/sims_prioritization_template.xlsx)), and (3) the folder path to where you want the operating unit SIMS prioritizatoin file saved. Leaving the last two parameters blank will just store the scores to R's memory and not export them to the template.

```{r}
#parameters - adjust to reflext location on your machine
  filepath <- "~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20180921_v2_2_PEPFARLandia.rds"
  template_filepath <- "~/SIMS/Template/asSIMSble/Products/sims_prioritization_template.xlsx"
  output_folderpath <- "~/SIMS/Products/"
  
# run and export to template
  assemble(filepath, template_filepath, output_folderpath)
  
#run, storing in R's memory
  df_scores <- assemble(filepath)
```
To run this against multiple OUs, you can take advantage of `purrr::map()`.

```{r}
#run for multiple countries
#install purrr if it's not currently installed (included in the tidyverse package)
  install.packages("purrr")
  library(purrr)
  
#list files
  files <- c("~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20180921_v2_2_Westeros.rds",
             "~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20180921_v2_2_Essos.rds",
             "~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20180921_v2_2_Sothoryos.rds")
#process multiple template creations
  map(.x = files,
      .f = ~ assemble(.x, template_filepath, output_folderpath))

```
For more information on the indicators, you can take a look at the [wiki](https://github.com/USAID-OHA-SI/asSIMSble/wiki) for more on indicators. Additionally, site data and sample data can be found on [PEPFAR Sharepoint](https://www.pepfar.net/Project-Pages/collab-39/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2FProject-Pages%2Fcollab-39%2FShared%20Documents%2FSIMS%20ST3%20October%202018%2FSite%20Selection%20Tool&FolderCTID=0x012000239C68CD65B2DD4F9FA0D4A3A95250C8&View=%7BE0D53BE1-C8A2-458C-A953-ED31C1F42C50%7D).
