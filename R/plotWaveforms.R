#' @title plotWaveforms
#' @description Plots imported waveform data
#' @details This functions takes recorded oscilloscope data (waveforms)
#' (obtained through getWaveforms.R) and plots it
#' @aliases plotwaveforms
#' @author Kai Budde-Sagert
#' @export plotWaveforms
#' @param input_data A tibble (measurement data)
#' @param output_dir A character (path to output directory)
#' @param show_time_in_us A boolean (indicates whether time is to be shown
#' in us)
#' @param channel_stimulation_pulse A character (name of the channel with
#' the stimulation pulse)
#' @param channel_function_generator A character (name of the channel with
#' the function generator pulse (for GATE TTL))
#' @param channel_resistor A character (name of the channel that recorded the
#' voltage drop across a shunt resistor)
#' @param voltage_limits_of_plot A number (value of min (negativ of the value)
#' and max values to be shown on y axis)
#' @param plot_waveforms A character (shows if all, none, or one waveform shall be plotted)
#' @param filter_stim_off A Boolean (indicates whether measurements while
#' stim==off are to be filtered)
#' @param shunt_resistance A number (Resistance in ohm)
#' @param plot_current_calculation A Boolean (plot min and max of measurements
#' using the ohmic resistance of the shunt resistor)
#' @param y_position_current_text A number (y position for displaying current
#' measurement text).
#' @param epsilon_for_filtering A number (min. distance for mean from 0 needed for filtering out the signal generator)
#' @param plot_title A character (title of the plot)
#' @param print_width A number (width in cm of the saved plot)
#' @param print_height A number (height in cm of the saved plot)
#' @return 0

# Created: 2022/04/21
# Last changed: 2023/08/01

plotWaveforms <- function(input_data = NULL,
                          output_dir = NULL,
                          show_time_in_us = FALSE,
                          channel_function_generator = "CHAN1",
                          channel_stimulation_pulse = "CHAN2",
                          channel_resistor = NA,
                          plot_waveforms = "all",
                          voltage_limits_of_plot = NULL,
                          filter_stim_off = TRUE,
                          shunt_resistance = NA,
                          plot_current_calculation = FALSE,
                          y_position_current_text = NA,
                          epsilon_for_filtering = 1.5,
                          plot_title = NULL,
                          print_width = 19.2,
                          print_height = 10.8) {

  # Some function parameters
  factor_for_min_max_scaling <- 1.1

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



  # Convert time to \mu s ##################################################
  if(show_time_in_us){
    # Time is given in s
    input_data$time <- input_data$time*1e6
  }

  # Min and Max for plotting (avoid outliers)
  global_max_value <- as.numeric(quantile(input_data$U[input_data$Channel == channel_stimulation_pulse], 0.99, na.rm = TRUE))
  global_min_value <- as.numeric(quantile(input_data$U[input_data$Channel == channel_stimulation_pulse], 0.01, na.rm = TRUE))

  if(is.null(voltage_limits_of_plot)){
    voltage_limits_of_plot <- factor_for_min_max_scaling *
      max(abs(global_max_value), abs(global_min_value))
  }

  # Detect frequency of generator pulses ###################################

  # Delete data without signal from frequency generator
  df_dummy <- input_data %>%
    dplyr::filter(Channel == channel_function_generator) %>%
    dplyr::group_by(ID) %>%
    # dplyr::summarise(mean = mean(U, na.rm=TRUE))
    dplyr::summarise(p2p = max(U, na.rm=TRUE)-min(U, na.rm=TRUE))

  ID_with_frequency_signal <- df_dummy$ID[df_dummy$p2p > epsilon_for_filtering & !is.na(df_dummy$p2p)][1]

  filter_date <- unique(input_data$date_time[input_data$ID == ID_with_frequency_signal])[1]
  df_dummy <- input_data %>%
    dplyr::filter(Channel == channel_function_generator,
                  date_time == filter_date)

  # rising_edges <- which(df_dummy$U < mean(df_dummy$U))[
  #   (which(df_dummy$U < mean(df_dummy$U)) + 1) %in%
  #     which(df_dummy$U > mean(df_dummy$U))]

  rising_edges <- which(df_dummy$U < max(df_dummy$U, na.rm = TRUE)/2)[
    ( which(df_dummy$U < max(df_dummy$U, na.rm = TRUE)/2) + 1 ) %in%
      which(df_dummy$U > max(df_dummy$U, na.rm = TRUE)/2) ]

  start_point_function_generator_pulse <- df_dummy$time[rising_edges[1]]
  end_point_function_generator_pulse <- df_dummy$time[rising_edges[length(rising_edges)]]

  # Calculate frequency of
  if(start_point_function_generator_pulse == end_point_function_generator_pulse){
    print("The frequency cannot be determined because you do not have recorded the full period of the function generator.")
  }else{
    periods <- length(rising_edges)-1
    frequency_of_function_generator <- periods/(end_point_function_generator_pulse-start_point_function_generator_pulse)

    print(paste("The frequency of the function generator was: ",
                frequency_of_function_generator, " Hz.", sep=""))
  }


  # Filter data for stimulation pulses #####################################
  df_dummy <- input_data %>%
    dplyr::filter(Channel == channel_stimulation_pulse) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(PeakToPeak=max(U)-min(U))

  ID_without_stim <- df_dummy$ID[df_dummy$PeakToPeak < epsilon_for_filtering]

  if(filter_stim_off){
    input_data <- input_data %>%
      dplyr::filter(!(input_data$ID %in% ID_without_stim))
  }

  if(!(nrow(input_data) > 0)){
    print("Nothing to be plotted.")
    return(invisible(NULL))
  }

  # Plot each waveform recording in a separate image #######################
  if(plot_waveforms == "all"){
    waveforms <-  unique(input_data$ID)
  }else if(plot_waveforms == "one"){
    which_wave <- max(floor(length(unique(input_data$ID))/2), 1)
    waveforms <-  unique(input_data$ID)[which_wave]
  }else{
    waveforms <- NA
  }

  if(length(waveforms) > 1 || !is.na(waveforms)){
    for(i in waveforms){

      input_data_filtered <- input_data %>%
        dplyr::filter(ID == i)

      # Get max and min (smoothed) of all stimulation pulses #################
      lower_bound <- (max(input_data_filtered$U[input_data_filtered$Channel == channel_stimulation_pulse]) -
                        mean(input_data_filtered$U[input_data_filtered$Channel == channel_stimulation_pulse]))/2
      upper_bound <- (min(input_data_filtered$U[input_data_filtered$Channel == channel_stimulation_pulse]) -
                        mean(input_data_filtered$U[input_data_filtered$Channel == channel_stimulation_pulse]))/2

      max_value <- as.numeric(quantile(input_data_filtered$U[
        input_data_filtered$Channel == channel_stimulation_pulse & input_data_filtered$U > lower_bound],
        0.99, na.rm = TRUE))
      min_value <- as.numeric(quantile(input_data_filtered$U[
        input_data_filtered$Channel == channel_stimulation_pulse & input_data_filtered$U < upper_bound],
        0.01, na.rm = TRUE))

      p2p_value <- abs(max_value) + abs(min_value)
      if(is.na(p2p_value)){
        p2p_value <- 0
      }
      p2p_value <- format(round(p2p_value, digits = 1), nsmall = 1)
      p2p_value <- paste("V_p2p=", p2p_value, "V", sep="")

      max_value_text <- paste("V_max=", format(round(max_value, digits = 1), nsmall = 1), "V", sep="")
      min_value_text <- paste("V_min=", format(round(min_value, digits = 1), nsmall = 1), "V", sep="")

      # Get max and min (smoothed) of resistor voltages ####################
      if(!is.na(channel_resistor) && !is.na(shunt_resistance)){
        lower_bound <- (max(input_data_filtered$U[input_data_filtered$Channel == channel_resistor]) -
                          mean(input_data_filtered$U[input_data_filtered$Channel == channel_resistor]))/2
        upper_bound <- (min(input_data_filtered$U[input_data_filtered$Channel == channel_resistor]) -
                          mean(input_data_filtered$U[input_data_filtered$Channel == channel_resistor]))/2

        max_value_resistor <- as.numeric(quantile(input_data_filtered$U[
          input_data_filtered$Channel == channel_resistor & input_data_filtered$U > lower_bound],
          0.99, na.rm = TRUE))
        min_value_resistor <- as.numeric(quantile(input_data_filtered$U[
          input_data_filtered$Channel == channel_resistor & input_data_filtered$U < upper_bound],
          0.01, na.rm = TRUE))

        p2p_value_resistor <- abs(max_value_resistor) + abs(min_value_resistor)
        if(is.na(p2p_value_resistor)){
          p2p_value_resistor <- 0
        }
        p2p_value_resistor <- format(round(p2p_value_resistor, digits = 1), nsmall = 1)
        p2p_value_resistor <- paste("V_p2p=", p2p_value_resistor, "V", sep="")

        max_resistor_value_text <- paste("I(+)=", format(round(max_value_resistor / shunt_resistance * 1000), nsmall = 1), "mA", sep="")
        min_resistor_value_text <- paste("I(-)=", format(round(abs(min_value_resistor) / shunt_resistance * 1000, digits = 1), nsmall = 1), "mA", sep="")
      }


      # Calculate mean value of stimulation pulses ###########################

      if(start_point_function_generator_pulse == end_point_function_generator_pulse){
        df_dummy <- input_data_filtered %>%
          dplyr::filter(Channel == channel_stimulation_pulse)

        mean_value <- mean(df_dummy$U)
      }else{
        df_dummy <- input_data_filtered %>%
          dplyr::filter(Channel == channel_stimulation_pulse) %>%
          dplyr::filter(time >= start_point_function_generator_pulse &
                          time < end_point_function_generator_pulse)

        mean_value <- mean(df_dummy$U)
      }

      mean_value_value_text <- paste("V_mean=", format(round(mean_value, digits = 1), nsmall = 1), "V", sep="")


      # Plot complete data from YAML file ####################################
      plot_annotation_x <- 0.5*max(input_data_filtered$time)
      plot_annotation_x_minmax <- 0.1*max(input_data_filtered$time)
      xaxis_lab <- ifelse(test = show_time_in_us,yes = "time/\U00B5s", no = "time/s")

      plot_waveform <- ggplot2::ggplot(data = input_data_filtered,
                                       aes(x = time, y = U, color=Channel)) +
        geom_line() +
        geom_hline(yintercept=max_value, linetype="dashed", color = "darkgray", size=1) +
        geom_hline(yintercept=min_value, linetype="dashed", color = "darkgray", size=1) +
        geom_hline(yintercept=mean_value, linetype="dotdash", color = "darkgray", size=1) +
        annotate("text", x=plot_annotation_x, y=(voltage_limits_of_plot-1), label=p2p_value, color = "darkgray") +
        annotate("text", x=plot_annotation_x_minmax, y=(max_value-1), label=max_value_text, color = "darkgray") +
        annotate("text", x=plot_annotation_x_minmax, y=(min_value+1), label=min_value_text, color = "darkgray") +
        annotate("text", x=plot_annotation_x, y=-0.5, label=mean_value_value_text, color = "darkgray") +
        coord_cartesian(ylim = c(-voltage_limits_of_plot, voltage_limits_of_plot)) +
        # labs(title=paste(plot_title_without_date, input_data_filtered$date_time[1], sep=" "),
        #      x = xaxis_lab, y = "U/V") +
        labs(title=paste(plot_title, format(as.POSIXct(input_data_filtered$date_time[1]), format = "%H:%M:%S"), sep=" "),
             x = xaxis_lab, y = "U/V") +
        theme_bw() +
        theme(axis.text.x = element_text(angle=90, vjust = 0.5),
              plot.title = element_text(hjust = 0.5))

      if(plot_current_calculation){
        plot_annotation_resistor_x <- 0.9*max(input_data_filtered$time)

        if(is.na(y_position_current_text)){
          y_position_current_text <- 1.1*max_value_resistor
        }

        plot_waveform <- plot_waveform +
          geom_hline(yintercept=max_value_resistor, linetype="dotted", color = "darkgoldenrod", size=1) +
          geom_hline(yintercept=min_value_resistor, linetype="dotted", color = "darkgoldenrod", size=1) +
          annotate("text", x=plot_annotation_resistor_x, y=y_position_current_text, label=max_resistor_value_text, color = "darkgoldenrod") +
          annotate("text", x=plot_annotation_resistor_x, y=-y_position_current_text, label=min_resistor_value_text, color = "darkgoldenrod")
      }

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

  # Plot all waveforms in one image ########################################

  # Get max and min (smoothed) of all stimulation pulses #################
  lower_bound <- (max(input_data$U[input_data$Channel == channel_stimulation_pulse]) -
                    mean(input_data$U[input_data$Channel == channel_stimulation_pulse]))/2
  upper_bound <- (min(input_data$U[input_data$Channel == channel_stimulation_pulse]) -
                    mean(input_data$U[input_data$Channel == channel_stimulation_pulse]))/2

  max_value <- as.numeric(quantile(input_data$U[
    input_data$Channel == channel_stimulation_pulse & input_data$U > lower_bound],
    0.99, na.rm = TRUE))
  min_value <- as.numeric(quantile(input_data$U[
    input_data$Channel == channel_stimulation_pulse & input_data$U < upper_bound],
    0.01, na.rm = TRUE))

  p2p_value <- abs(max_value) + abs(min_value)

  if(is.na(p2p_value)){
    p2p_value <- 0
  }
  p2p_value <- format(round(p2p_value, digits = 1), nsmall = 1)
  p2p_value <- paste("V_p2p=", p2p_value, "V", sep="")

  max_value_text <- paste("V_max=", format(round(max_value, digits = 1), nsmall = 1), "V", sep="")
  min_value_text <- paste("V_min=", format(round(min_value, digits = 1), nsmall = 1), "V", sep="")


  # Calculate mean
  if(start_point_function_generator_pulse == end_point_function_generator_pulse){
    df_dummy <- input_data %>%
      dplyr::filter(Channel == channel_stimulation_pulse)

    mean_value <- mean(df_dummy$U)
  }else{
    df_dummy <- input_data %>%
      dplyr::filter(Channel == channel_stimulation_pulse) %>%
      dplyr::filter(time >= start_point_function_generator_pulse &
                      time < end_point_function_generator_pulse)

    mean_value <- mean(df_dummy$U)
  }

  mean_value_value_text <- paste("V_mean=", format(round(mean_value, digits = 1), nsmall = 1), "V", sep="")




  # Plot complete data from YAML file ######################################
  plot_annotation_x <- 0.5*max(input_data$time)
  plot_annotation_x_minmax <- 0.1*max(input_data$time)
  xaxis_lab <- ifelse(test = show_time_in_us,yes = "time/\U00B5s", no = "time/s")

  start_time <- min(input_data$date_time)
  input_data$time_of_experiment_in_minutes <- as.double(difftime(
    input_data$date_time, start_time, units = "mins"))

  plot_waveform <- ggplot2::ggplot(data = input_data) +
    scattermore::geom_scattermore(aes(x = time, y = U, color=time_of_experiment_in_minutes),
                                  data = . %>% filter(Channel == channel_stimulation_pulse),
                                  alpha = 1, pointsize = 0.6)+
    scale_color_viridis_c(name = "Time of\nexperiment\nin min.", direction = -1, limits = c(0, 120), option = "C") +
    geom_line(aes(x = time, y = U, linetype = "channel_function_generator"), color = "#00BFC4",
              data = . %>% filter(Channel == channel_function_generator) %>% group_by(time) %>% mutate(U=mean(U)),
              linewidth=0.6) +
    geom_line(aes(x = time, y = U, linetype = "channel_stimulation_pulse"), color = "#F8766D",
              data = . %>% filter(Channel == channel_stimulation_pulse) %>% group_by(time) %>% mutate(U=mean(U)),
              linewidth=0.6) +
    scale_linetype_manual(name = "Channel", values=c("channel_function_generator" = "solid", "channel_stimulation_pulse" = "dashed"), labels = c("FunGen", "ES (mean)")) +
    guides(linetype = guide_legend(override.aes = list(color=c("#00BFC4", "#F8766D"), linetype=c(1,2)), order = 1 )) +
    geom_hline(yintercept=max_value, linetype="dashed", color = "darkgray") +
    geom_hline(yintercept=min_value, linetype="dashed", color = "darkgray") +
    geom_hline(yintercept=mean_value, linetype="dotdash", color = "darkgray", size=1) +
    annotate("text", x=plot_annotation_x, y=(voltage_limits_of_plot-1), label=p2p_value, color = "darkgray") +
    annotate("text", x=plot_annotation_x_minmax, y=(max_value+1), label=max_value_text, color = "darkgray") +
    annotate("text", x=plot_annotation_x_minmax, y=(min_value-1), label=min_value_text, color = "darkgray") +
    annotate("text", x=plot_annotation_x, y=-0.5, label=mean_value_value_text, color = "darkgray")  +
    coord_cartesian(ylim = c(-voltage_limits_of_plot, voltage_limits_of_plot)) +
    labs(title=plot_title,
         x = xaxis_lab, y = "U/V") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))

  # Save files
  file_path <- file.path(output_dir, paste0(name_for_saving, "_all_in_one.png"))
  ggsave(plot = plot_waveform, filename = file_path, device = "png", width = print_width, height = print_height,  units = "cm")

  return(invisible(NULL))
}

