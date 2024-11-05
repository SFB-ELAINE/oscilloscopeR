#' @title plotWaveforms_simple
#' @description Plots imported waveform data without any calculations
#' @details This functions takes recorded oscilloscope data (waveforms)
#' (obtained through getWaveforms.R) and plots it
#' @aliases plotwaveformssimple, plotwaveforms_simple
#' @author Kai Budde-Sagert
#' @export plotWaveforms_simple
#' @param input_data A tibble (measurement data)
#' @param output_dir A character (path to output directory)
#' @param voltage_limits_of_plot A number (value of min (negativ of the value)
#' and max values to be shown on y axis)
#' @param plot_title A character (title of the plot)
#' @param print_width A number (width in cm of the saved plot)
#' @param print_height A number (height in cm of the saved plot)
#' @return 0

# Created: 2024/11/05
# Last changed: 2024/11/05

plotWaveforms_simple <- function(input_data = NULL,
                          output_dir = NULL,
                          voltage_limits_of_plot = NULL,
                          plot_title = NULL,
                          print_width = 19.2,
                          print_height = 10.8) {
  
  # Check for input data
  if(is.null(input_data)){
    print("Please call function with input data.")
    return()
  }
  
  # Check for output directory
  if(is.null(output_dir)){
    output_dir <- getwd()
    output_dir <- file.path(output_dir, "output")
    dir.create(output_dir, showWarnings = FALSE)
    print("The plots will we saves here:")
    print(output_dir)
  }
  
  # Set plot title
  if(is.null(plot_title)){
    plot_title <- ""
    # plot_title_without_date <- ""
    
    # Get date of experiment
    date_of_experiment <- unique(as.Date(input_data$date_time))[1]
    
    name_for_saving <- date_of_experiment
    name_for_saving_without_date <- ""
    
  }else{
    name_for_saving <- gsub(pattern = " ", replacement = "_", x = plot_title)
    
    plot_title_without_date <- substr(x = plot_title, start = 12, stop = nchar(plot_title))
    name_for_saving_without_date <- gsub(pattern = " ", replacement = "_", x = plot_title_without_date)
  }
  
  # Min and Max for plotting (avoid outliers)
  global_max_value <- as.numeric(quantile(input_data$U, 0.99, na.rm = TRUE))
  global_min_value <- as.numeric(quantile(input_data$U, 0.01, na.rm = TRUE))
  
  if(is.null(voltage_limits_of_plot)){
    voltage_limits_of_plot_min <- 1.1 * global_min_value
    voltage_limits_of_plot_max <- 1.1 * global_max_value
  }
  
  # Plot each waveform recording in a separate image #######################
  waveforms <-  unique(input_data$ID)
  
  if(length(waveforms) > 1 || !is.na(waveforms)){
    for(i in waveforms){
      
      input_data_filtered <- input_data %>%
        dplyr::filter(ID == i)
      
      xaxis_lab <- "time/s"
      
      plot_waveform <- ggplot2::ggplot(data = input_data_filtered,
                                       aes(x = time, y = U, color=Channel)) +
        geom_line() +
        coord_cartesian(ylim = c(voltage_limits_of_plot_min, voltage_limits_of_plot_max)) +
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        # labs(title=paste(plot_title_without_date, input_data_filtered$date_time[1], sep=" "),
        #      x = xaxis_lab, y = "U/V") +
        labs(title=paste(plot_title, format(as.POSIXct(input_data_filtered$date_time[1]), format = "%H:%M:%S"), sep=" "),
             x = xaxis_lab, y = "U/V") +
        theme_bw() +
        theme(axis.text.x = element_text(angle=90, vjust = 0.5),
              plot.title = element_text(hjust = 0.5))
      
      # Date and time as writable string
      date_time_measurement <- input_data_filtered$date_time[1]
      date_time_measurement <- gsub(pattern = " ", replacement = "_", x = date_time_measurement)
      date_time_measurement <- gsub(pattern = ":", replacement = "", x = date_time_measurement)
      
      # Save files
      file_path <- file.path(output_dir, paste0(date_time_measurement, "_", name_for_saving_without_date, ".png"))
      ggsave(plot = plot_waveform, filename = file_path, device = "png", width = print_width, height = print_height,  units = "cm")
    }
    
    rm(i)
    
  }
  
  return(invisible(NULL))
}

