# Testscript for using the R package oscilloscopeR for development  ++++++++++++++
# Author: Kai Budde
# Created: 2022/04/19
# Last changed: 2022/04/21

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "yaml", "ggplot2", "devtools", "scattermore")
groundhog.library(pkgs, groundhog.day)


# Package development ######################################################

# Check package
# devtools::check()

# Document package
devtools::document()

# Load package to use it
devtools::load_all()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# YAML File
input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/IonOptixMeasurements/1_GateOut/20211203-110117-measurement.yml"
input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/IonOptixMeasurements/1_GateOut/20211203-110117-wave.yml"

# ZIP File (containing yaml files)
input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/CellBioShortStimI.zip"

# Directory (containing yaml files)
input_directory <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/01_hotMedium_hotElectrodes"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

### Test small functions ---------------------------------------------------

# Get Vavg (average voltage) and Vpp (peak-to-peak voltage) and plot the data
df_data <- getMeasurments(input_file = input_file)
df_data <- getMeasurments(input_directory = input_directory)
plotMeasurements(input_data = df_data)

# Get Waveforms and plot the data
df_data <- getWaveforms(input_file = input_file)
df_data <- getWaveforms(input_directory = input_directory)
plotWaveforms(input_data = df_data, channel_function_generator = "CHAN2",
              channel_stimulation_pulse = "CHAN1")
