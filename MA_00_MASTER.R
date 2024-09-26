# master

rm(list = ls())



# Load Settings.
if (Sys.info()[["user"]] == "tillm") {
  
  source("C:/Users/tillm/OneDrive/Desktop/Masterarbeit/scripts/MA_01_SETTINGS.R")
  
} else if (Sys.info()[["user"]] == "Privat") {
  
  source("C:/Users/Privat/Desktop/Masterarbeit/scripts/MA_01_SETTINGS.R")
}


# Read functions.
source(str_c(path$script_folder,"MA_02_LOAD_DATA.R"))

# Clean that data.
source(str_c(path$script_folder,"MA_03_CLEAN_DATA.R"))

# Calculate all important variables.
source(str_c(path$script_folder,"MA_04_VARIABLES.R"))

# Gimme summary stats.
source(str_c(path$script_folder,"MA_05_SUMMARYSTATS.R"))

# Run dem regressions.
source(str_c(path$script_folder,"MA_06_REGRESSIONS.R"))
