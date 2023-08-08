#' @title plotMeasurements
#' @description Plots imported measurment data
#' @details This functions takes recorded oscilloscope data (Vavg and Vpp)
#' (obtained through getMeasurements.R) and plots it
#' @aliases plotmeasurements
#' @author Kai Budde
#' @export plotMeasurements
#' @param input_data A tibble (measurement data)
#' @param output_dir A character (path to output directory)
#' @param vavg_min A number (min value of Vavg to be plotted)
#' @param vavg_max A number (max value of Vavg to be plotted)
#' @param vpp_min A number (min value of Vpp to be plotted)
#' @param vpp_max A number (max value of Vpp to be plotted)
#' @param plot_title A character (title of the plot)
#' @return 0

# Created: 2022/04/20
# Last changed: 2023/02/21

plotMeasurements <- function(input_data = NULL,
                             output_dir = NULL,
                             vavg_min = NULL,
                             vavg_max = NULL,
                             vpp_min = NULL,
                             vpp_max = NULL,
                             plot_title = NULL) {

  # Some function parameters
  factor_for_min_max_scaling <- 1.1

  # Check for input data
  if(is.null(input_data)){
    print("Please call function with input data.")
    return(invisible(NULL))
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

    # Get date of experiment
    date_of_experiment <- unique(as.Date(input_data$date_time))[1]

    name_for_saving <- date_of_experiment
  }else{
    name_for_saving <- gsub(pattern = " ", replacement = "_", x = plot_title)
  }



  # Determine Vavg minimum and maximum if not given
  if(is.null(vavg_min)){
    vavg_min <- round(
      as.numeric(quantile(df_data$VAVG, 0.01, na.rm = TRUE)) *
        factor_for_min_max_scaling, digits = 0)
  }
  if(is.null(vavg_max)){
    vavg_max <- round(
      as.numeric(quantile(df_data$VAVG, 0.99, na.rm = TRUE)) *
        factor_for_min_max_scaling, digits = 0)
  }

  # Determine Vpp minimum and maximum if not given
  if(is.null(vpp_min)){
    vpp_min <- round(
      as.numeric(quantile(df_data$VPP, 0.01, na.rm = TRUE)) *
        factor_for_min_max_scaling, digits = 0)
  }
  if(is.null(vpp_max)){
    vpp_max <- round(
      as.numeric(quantile(df_data$VPP, 0.99, na.rm = TRUE)) *
        factor_for_min_max_scaling, digits = 0)
  }

  # # Set time breaks
  #
  # timedate_breaks <- max(df_data$date_time)-min(df_data$date_time)
  #
  # time_unit <- attributes(timedate_breaks)$units
  # timedate_breaks <- as.numeric(timedate_breaks)
  #
  # use_time_breaks <- TRUE
  #
  # if(timedate_breaks > 10){
  #   timedate_breaks <- round(timedate_breaks / 10)
  # }else if(timedate_breaks!=0){
  #   timedate_breaks <- round(timedate_breaks / 10, digits = 2)
  # }else{
  #   use_time_breaks <- FALSE
  # }
  #
  # if(use_time_breaks){
  #   if(time_unit == "years"){
  #     if(timedate_breaks < 1){
  #       timedate_breaks <- paste(timedate_breaks*30, "months", sep=" ")
  #     }else{
  #       timedate_breaks <- paste(timedate_breaks, "years", sep=" ")
  #     }
  #   }else if(time_unit == "months"){
  #     if(timedate_breaks < 1){
  #       timedate_breaks <- paste(timedate_breaks*30, "days", sep=" ")
  #     }else{
  #       timedate_breaks <- paste(timedate_breaks, "months", sep=" ")
  #     }
  #   }else if(time_unit == "days"){
  #     if(timedate_breaks < 1){
  #       timedate_breaks <- paste(timedate_breaks*24, "hours", sep=" ")
  #     }else{
  #       timedate_breaks <- paste(timedate_breaks, "days", sep=" ")
  #     }
  #   }else if(time_unit == "hours"){
  #     if(timedate_breaks < 1){
  #       timedate_breaks <- paste(timedate_breaks*60, "mins", sep=" ")
  #     }else{
  #       timedate_breaks <- paste(timedate_breaks, "hours", sep=" ")
  #     }
  #   }else if(time_unit == "mins"){
  #     if(timedate_breaks < 1){
  #       timedate_breaks <- paste(timedate_breaks*60, "secs", sep=" ")
  #     }else{
  #       timedate_breaks <- paste(timedate_breaks, "mins", sep=" ")
  #     }
  #   }else if(time_unit == "secs"){
  #     timedate_breaks <- paste(timedate_breaks, "secs", sep=" ")
  #   }
  #
  # }

  # Plot VAVG ################################################################
  plot_vavg <- ggplot2::ggplot(data = df_data,
                               aes(x = date_time, y = VAVG, group=Channel, color=Channel)) +
    geom_point() +
    coord_cartesian(ylim = c(vavg_min, vavg_max)) +
    scale_y_continuous(breaks = seq(vavg_min, vavg_max, by = 0.2),
                       minor_breaks = seq(vavg_min, vavg_max, by = 0.1)) +
    scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
    labs(title=plot_title, x = "Date", y = "U_avg/V") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))

  # if(use_time_breaks){
  #   plot_vavg <- plot_vavg +
  #     scale_x_datetime(date_breaks = timedate_breaks)
  # }

  # Save files
  file_path <- file.path(output_dir, paste0(name_for_saving, "_vavg_measurement.png"))
  ggsave(plot = plot_vavg, filename = file_path, device = "png", width = 19.2, height = 10.8,  units = "cm")

  # Find VPP #################################################################

  plot_vpp <- ggplot2::ggplot(data = df_data, aes(x = date_time, y = VPP, group=Channel, color=Channel)) +
    geom_point() +
    coord_cartesian(ylim = c(vpp_min, vpp_max)) +
    scale_y_continuous(breaks = seq(vpp_min, vpp_max, by = 2),
                       minor_breaks = seq(vpp_min, vpp_max, by = 1)) +
    scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
    labs(title=plot_title, x = "Date", y = "U_p2p/V") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))

  # if(use_time_breaks){
  #   plot_vpp <- plot_vpp +
  #     scale_x_datetime(date_breaks = timedate_breaks)
  # }

  file_path <- file.path(output_dir, paste0(name_for_saving, "_vp2p_measurement.png"))
  ggsave(plot = plot_vpp, filename = file_path, device = "png", width = 19.2, height = 10.8,  units = "cm")

  return(invisible(NULL))
}

