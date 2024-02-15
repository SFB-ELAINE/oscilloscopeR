# Testscript for using the R package oscilloscopeR for development  ++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2022/04/19
# Last changed: 2024/02/14

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2023-01-01"

if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
# Load packages
library(groundhog)
pkgs <- c("tidyverse", "yaml", "scattermore", "devtools", "lubridate")
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
# ZIP File (containing yaml files)
input_zip_file <- file.path("inst", "testdata","recorded_stimulation.zip")
input_zip_file <- file.path("inst", "testdata","S16_20230323_ShortStimCellBio_MediumMediated.zip")
# # YAML File
# # input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/IonOptixMeasurements/1_GateOut/20211203-110117-measurement.yml"
# # input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/IonOptixMeasurements/1_GateOut/20211203-110117-wave.yml"
#
# # ZIP File (containing yaml files)
# input_file <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/CellBioShortStimI.zip"
#
# # Directory (containing yaml files)
# input_directory <- "C:/Users/Kai/Documents/git/gitHub/OscilloscopeRecordings/data/01_hotMedium_hotElectrodes"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Measurement data ---------------------------------------------------------
# Zip file
df_data <- getMeasurements(input_file = input_zip_file)
# Plot measurement data
plotMeasurements(input_data = df_data)


# Waveform data ------------------------------------------------------------
df_data <- getWaveforms(input_file = input_zip_file)

# Plot wave form data
plotWaveforms(input_data = df_data,
              plot_waveforms = "all")














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


### Plotting IonOptix Measurements ---------------------------------------------------

# Version of R package oscilloscopeR: 0.1.0
input_directory <- "C:/Users/Kai/Documents/git/gitHub/SI_IonOptix_Measurements/IonOptixMeasurements/1_GateOut"
df_data <- getMeasurements(input_directory = input_directory)
plotMeasurements(input_data = df_data)

df_data <- getWaveforms(input_directory = input_directory)
plotWaveforms(input_data = df_data, epsilon = 0.01, channel_stimulation_pulse = "CHAN1",
              channel_function_generator = "CHAN2")


input_directory <- "C:/Users/Kai/Documents/git/gitHub/SI_IonOptix_Measurements/IonOptixMeasurements/2_GateAndAux"
df_data <- getMeasurements(input_directory = input_directory)
plotMeasurements(input_data = df_data)

df_data <- getWaveforms(input_directory = input_directory)
plotWaveforms(input_data = df_data, epsilon = 0.01, channel_stimulation_pulse = "CHAN1",
              channel_function_generator = "CHAN2")


input_directory <- "C:/Users/Kai/Documents/git/gitHub/SI_IonOptix_Measurements/IonOptixMeasurements/3_Reihenmessung"
df_data <- getMeasurements(input_directory = input_directory)
plotMeasurements(input_data = df_data)

df_data <- getWaveforms(input_directory = input_directory)
plotWaveforms(input_data = df_data, epsilon = 0.01, channel_stimulation_pulse = "CHAN1",
              channel_function_generator = "CHAN2")


input_directory <- "C:/Users/Kai/Documents/git/gitHub/SI_IonOptix_Measurements/IonOptixMeasurements/4_Breakoutbox_trocken/a1c1"
df_data <- getMeasurements(input_directory = input_directory)
plotMeasurements(input_data = df_data)

df_data <- getWaveforms(input_directory = input_directory)
plotWaveforms(input_data = df_data, epsilon = 0.01, channel_stimulation_pulse = "CHAN3",
              channel_function_generator = "CHAN2")

plot_waveform <- ggplot2::ggplot(data = df_data,
                                 aes(x = time, y = U, color=Channel)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))


