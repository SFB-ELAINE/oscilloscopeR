#' @title getWaveforms
#' @description Imports waveform data from oscilloscope recordings
#' @details This function reads yaml files (from a directory) or a zip file
#' and imports the waveform data into a tibble
#' @aliases getwaveforms
#' @author Kai Budde
#' @export getWaveforms
#' @param input_file A character (path to yaml file to be converted)
#' @param input_directory A character (path to directory with yaml files)
#' @return A tibble with the data

# Created: 2022/04/21
# Last changed: 2022/04/21

getWaveforms <- function(input_file = NULL, input_directory = NULL) {

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
      if(grepl(pattern = "wave\\.yml", x = input_file, ignore.case = TRUE)){
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
    yaml_files <- yaml_files[grepl(pattern = "wave\\.yml", x = yaml_files, ignore.case = TRUE)]
  }else if(yml_directory){
    yaml_files <- list.files(path = input_directory, pattern = "wave\\.yml")
  }else{
    yaml_files <- input_file
  }

  for(i in 1:length(yaml_files)){
    # Read data
    if(zip_container){
      df_data_list <- yaml::read_yaml(file = unz(
        description = file.path(input_directory, input_file),
        filename = yaml_files[i] ))
    }else{
      df_data_list <- yaml::read_yaml(file = file.path(input_directory, yaml_files[i]))
    }

    # Convert list to tibble
    df_data_dummy <- dplyr::as_tibble(df_data_list)

    # Get date and time of measurement
    df_data_dummy$date_time <- gsub(pattern = "-wave\\.yml",
                                    replacement = "",
                                    x = basename(yaml_files[i]))

    df_data_dummy$ID <- i

    if(i == 1){
      df_data <- df_data_dummy
    }else{
      df_data <- dplyr::bind_rows(df_data, df_data_dummy)
    }

  }

  rm(list = c("df_data_dummy", "df_data_list", "i"))

  # Convert date and time
  df_data$date_time <- as.POSIXct(strptime(df_data$date_time, "%Y%m%d-%H%M%S"))

  # Put all voltages into one column and the respective channel into another one
  df_data <- tidyr::gather(data = df_data, key = "Channel", value = "U",  -c("time", "date_time", "ID"))

  return(df_data)
}
