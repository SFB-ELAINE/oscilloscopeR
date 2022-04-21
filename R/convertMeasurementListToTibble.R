#' @title convertMeasurementListToTibble
#' @description Converts list with data to tibble
#' @details This function takes a list with oscilloscope readings saved
#' as yaml and coverts it to a tibble
#' @aliases convertmeasurementlisttotibble
#' @author Kai Budde
#' @export convertMeasurementListToTibble
#' @param df_data_list A list (list of data)
#' @param variabel_name A character ()
#' @return A tibble with the data

# Created: 2022/04/19
# Last changed: 2022/04/19

convertMeasurementListToTibble <- function(df_data_list){

  for(j in 1:length(df_data_list)){
    if(j == 1){
      df_data <- dplyr::as_tibble(df_data_list[[j]])
      df_data$Channel <- gsub(pattern = "CHAN", replacement = "", x = names(df_data_list)[j])
    }else{
      df_data_dummy <- dplyr::as_tibble(df_data_list[[j]])
      df_data_dummy$Channel <- gsub(pattern = "CHAN", replacement = "", x = names(df_data_list)[j])

      df_data <- dplyr::bind_rows(df_data, df_data_dummy)
    }
  }

  return(df_data)
}
