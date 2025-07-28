#' @title getWaveforms
#' @description Imports waveform data from oscilloscope recordings
#' @details This function reads yaml files (from a directory) or a zip file
#' and imports the waveform data into a tibble
#' @aliases getwaveforms
#' @author Kai Budde-Sagert
#' @export getWaveforms
#' @param input_file A character (path to yaml file to be converted)
#' @param input_directory A character (path to directory with yaml files)
#' @param reduce_resolution_of_input_data A Boolean (showing if fewer point
#' are to be imported)
#' @return A tibble with the data

# Created: 2022/04/21
# Last changed: 2024/04/17

getWaveforms <- function(input_file = NULL,
                         input_directory = NULL,
                         reduce_resolution_of_input_data = TRUE) {

  # Check for null input
  if(is.null(input_file) & is.null(input_directory)){
    print("Please call function with input file or input directory.")
    return()
  }

  # Check if both input file and input directory are given
  if(!is.null(input_file) & !is.null(input_directory)){
    print("Please call function either with input file or with input directory.")
    return()
  }

  # Check that file is yaml file and not directory
  if(is.null(input_directory)){
    if(!grepl(pattern = "\\.yml|\\.zip", x = input_file, ignore.case = TRUE)){
      print("Please call function with a correct input file ('*.yml' or '*.zip').")
      return()
    }else{
      if(grepl(pattern = "wave.+yml", x = input_file, ignore.case = TRUE)){
        # A yml file is given
        yml_file <- TRUE
        yml_directory <- FALSE
        zip_container <- FALSE

        input_directory <- dirname(input_file)
        input_file <- basename(input_file)
      }else{
        # A zip container is given
        yml_file <- FALSE
        yml_directory <- FALSE
        zip_container <- TRUE

        input_directory <- dirname(input_file)
        input_file <- basename(input_file)
      }

    }
  }else{
    # A directory is given
    yml_file <- FALSE
    yml_directory <- TRUE
    zip_container <- FALSE
  }


  # Import data from YAML file #############################################
  #yaml_files <- list.files(path = directory_with_yaml_files, pattern = "measurement")
  # if(exists("zip_file_with_data") || exists("directory_with_zip_file")){
  #   zipped <- TRUE

  # Save true yaml files
  if(zip_container){
    yaml_files <- unzip(zipfile = file.path(input_directory, input_file), list = TRUE)
    yaml_files <- yaml_files$Name
    yaml_files <- yaml_files[grepl(pattern = "wave.+yml", x = yaml_files, ignore.case = TRUE)]
  }else if(yml_directory){
    yaml_files <- list.files(path = input_directory, pattern = "wave.+yml")
  }else{
    yaml_files <- input_file
  }

  if(length(yaml_files) == 0){
    print("No yaml files found.")
    return(NULL)
  }

  first_non_zero_data <- FALSE

  for(i in 1:length(yaml_files)){
    # Read data
    if(zip_container){
      df_data_list <- yaml::read_yaml(file = unz(
        description = file.path(input_directory, input_file),
        filename = yaml_files[i] ))
    }else{
      df_data_list <- yaml::read_yaml(file = file.path(input_directory, yaml_files[i]))
    }

    if(length(df_data_list[[1]]) > 0){

      # Convert list to tibble
      df_data_dummy <- dplyr::as_tibble(df_data_list)

      # Reduce dimension of input if required
      if(reduce_resolution_of_input_data){
        max_number_of_points <- 2000
        if(nrow(df_data_dummy) > max_number_of_points){
          # time_step <- min(diff(df_data_dummy$time))
          # time_interval <- max(df_data_dummy$time) - min(df_data_dummy$time)

          df_data_dummy <- df_data_dummy[seq(1, nrow(df_data_dummy), floor(nrow(df_data_dummy)/max_number_of_points)), ]
        }
      }


      # Get date and time of measurement
      df_data_dummy$date_time <- gsub(pattern = "-wave.+yml",
                                      replacement = "",
                                      x = basename(yaml_files[i]))

      # Get further information depending on file name
      if(length(grep(pattern = ".+-wave(.+)\\.yml", x = basename(yaml_files[i]), ignore.case = TRUE)) > 0){
        df_data_dummy$file_name_extension <- gsub(pattern = ".+-wave(.+)\\.yml",
                                                  replacement = "\\1",
                                                  x = basename(yaml_files[i]))
      }else{
        df_data_dummy$file_name_extension <- NA
      }

      # Add ID (file index)
      df_data_dummy$ID <- i

      # Put all voltages into one column and the respective channel into another one
      pivot_longer(c(x, y, z), names_to = "key", values_to = "value")

      df_data_dummy <- tidyr::pivot_longer(
        data = df_data_dummy,
        cols = -c("time", "date_time", "file_name_extension", "ID"),
        names_to = "Channel",
        values_to = "U")

      # Old version_
      # df_data_dummy <- tidyr::gather(data = df_data_dummy, key = "Channel", value = "U",  -c("time", "date_time", "file_name_extension", "ID"))
      # df_data_dummy <- df_data_dummy %>%
      #   dplyr::arrange(time, Channel)

      # Delete possible character of Channel column
      df_data_dummy$Channel <- as.numeric(gsub(pattern = "[^0-9.-]", replacement = "", x = df_data_dummy$Channel))

      if(i == 1 || first_non_zero_data){
        df_data <- df_data_dummy
        first_non_zero_data <- FALSE
      }else{
        df_data <- dplyr::bind_rows(df_data, df_data_dummy)
      }

    }else{
      if(i == 1){
        first_non_zero_data <- TRUE
      }
    }

  }

  rm(list = c("df_data_dummy", "df_data_list", "i"))

  # Convert date and time
  df_data$date_time <- as.POSIXct(strptime(df_data$date_time, "%Y%m%d-%H%M%S"))

  # Make channel number as factor
  df_data$Channel <- as.factor(df_data$Channel)

  return(df_data)
}
