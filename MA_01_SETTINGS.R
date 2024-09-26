# Set up

rm(list = ls())

##################
#### packages ####
##################

loadpackage <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
    }
    #  Load package (after installing)
    library( i , character.only = TRUE )
  }
}

#  Then try/install packages...
loadpackage( c("haven","tidyverse", "writexl",
               "magrittr","bannerCommenter",
               "readxl", "openxlsx", "plm",
               "stargazer", "car", "sandwich",
               "lmtest", "clubSandwich", "ggplot2"))

path <- list()

# Define paths
if (Sys.info()[["user"]] == "tillm") {
  
  path$data_folder <- "C:/Users/tillm/OneDrive/Desktop/Masterarbeit/data/EUKLEMS_INTANProd_Download/"
  path$script_folder <- "C:/Users/tillm/OneDrive/Desktop/Masterarbeit/scripts/"
  path$table_folder <- "C:/Users/tillm/OneDrive/Desktop/Masterarbeit/tables/"

} else if (Sys.info()[["user"]] == "Privat") {
  
  path$data_folder <- "C:/Users/Privat/Desktop/Masterarbeit/data/EUKLEMS_INTANProd_Download/"
  path$script_folder <- "C:/Users/Privat/Desktop/Masterarbeit/scripts/"
  path$table_folder <- "C:/Users/Privat/Desktop/Masterarbeit/tables/"
  
}

