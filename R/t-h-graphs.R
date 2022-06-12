#' set_options
#' @description Set options for downstream charts based on available data
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @export
set_options <- function(observations) {
  checkmate::assert_data_frame(observations)

  options(TEMP_ENABLE  = "temp"  %in% colnames(observations))
  options(HUM_ENABLE   = "hum"   %in% colnames(observations))
  options(CO2_ENABLE   = "co2"   %in% colnames(observations))
  options(LUX_ENABLE   = "lux"   %in% colnames(observations))
  options(DBA_ENABLE   = "dba"   %in% colnames(observations))
  options(VOC_ENABLE   = "voc"   %in% colnames(observations))
  options(HCHO_ENABLE  = "hcho"  %in% colnames(observations))
  options(PM1_ENABLE   = "pm1"   %in% colnames(observations))
  options(PM2_5_ENABLE = "pm2_5" %in% colnames(observations))
  options(PM10_ENABLE  = "pm10"  %in% colnames(observations))

}

#' monkey_palettes
#' @description define some color palettes and set them as R options to be globally available
#' @param devices data.frame, the wrangled devices data as output by `wrangle_devices`,
#' and further filtered by `remove_excluded_devices`
#' @export
monkey_palettes <- function(devices) {
  checkmate::assert_data_frame(devices)
  ## Colors with unikn package
  # https://cran.r-project.org/web/packages/unikn/vignettes/colors.html
  options(TEMP_COLOURS = unikn::usecol(pal = pal_seeblau, n = nrow(devices)))
  options(HUM_COLOURS  = unikn::usecol(pal = pal_pinky, n = nrow(devices)))
  options(CO2_COLOURS  = unikn::usecol(pal = pal_seegruen, n = nrow(devices)))
}

#' ts_chart
#' @description make a ggplotly time series chart for temperature, CO2, ...
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
#' @param target_variable character, the name of the column to use for plotting
#' Options are (for now): `temp`, `hum`
#' @examples
#' wrangled_devices <- wrangle_devices(test_params()$filtered_devices)
#' wrangled_weather <- wrangle_weather(
#'   filtered_weather = test_params()$filtered_weather,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp
#' )
#' wrangled_obs <- wrangle_observations(
#'   filtered_obs = test_params()$filtered_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp,
#'   devices = wrangled_devices,
#'   weather = wrangled_weather
#' )
#' data_volume <- get_data_volume(
#'   observations = wrangled_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp
#'   )
#' wrangled_devices <- remove_excluded_devices(
#'   target_data = wrangled_devices,
#'   data_volume = data_volume
#' )
#' wrangled_obs <- remove_excluded_devices(
#'   target_data = wrangled_obs,
#'   data_volume = data_volume
#' )
#' ts_chart(
#'   observations = wrangled_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp,
#'   target_variable = "temp"
#' )
#' ts_chart(
#'   observations = wrangled_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp,
#'   target_variable = "hum"
#' )
#' @export
ts_chart <-
  function(observations,
           from_timestamp,
           to_timestamp,
           target_variable) {
    checkmate::assert_number(from_timestamp)
    checkmate::assert_number(to_timestamp)
    checkmate::assert_data_frame(observations)

    green_zone <- make_green_area(observations)
    report_period <- report_period(from_timestamp , to_timestamp)
    target_colours <-
      getOption(paste0(toupper(target_variable), "_COLOURS"))

    y_lim <- switch(
      target_variable,
      "temp" = c(0, 30),
      "hum"  = c(30, 100),
      "co2"  = c(250, 2000)
    )

    y_lab <- switch(
      target_variable,
      "temp" = "Temperature (°C)",
      "hum"  = "Humidity (%)",
      "co2"  = "CO2 (ppm)"
    )

    t_lab <- switch(
      target_variable,
      "temp" = "Temperature Time Series ",
      "hum"  = "Rel. Humidity Time Series ",
      "co2"  = "Carbon Dioxide Time Series "
    )

    axis_high <- max(y_lim[[2]], max(as.numeric(as.numeric(observations[[target_variable]])), na.rm = TRUE))
    axis_low  <- min(y_lim[[1]],  min(as.numeric(as.numeric(observations[[target_variable]])), na.rm = TRUE))

    ts_chart <- observations %>%
      # TODO: these mutates should be temporary
      dplyr::mutate(temp = as.numeric(temp)) %>%
      dplyr::mutate(hum = as.numeric(hum)) %>%
      # mutate(name = paste(room, ":", device_id)) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = nzdt,
        y = !!rlang::sym(target_variable),
        colour = name
      )) +
      ggplot2::geom_polygon(
        data = green_zone,
        colour = "green",
        alpha = 0.10,
        fill = "green"
      ) +
      ggplot2::geom_line() +
      ggplot2::scale_color_manual(values = target_colours) +
      ggplot2::labs(title = paste0(t_lab, report_period),
                    colour = "Room") +
      ggplot2::ylab(y_lab) +
      ggplot2::xlab("Date") +
      ggplot2::ylim(axis_low, axis_high) +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = 10,
          angle = 60,
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title  = ggplot2::element_text(size = 13),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 11),
        legend.text = ggplot2::element_text(size = 10)
      )

    # Turn it interactive with ggplotly
    plotly::ggplotly(ts_chart, tooltip = c("x", "y", "colour")) %>%
      plotly::style(hoverinfo = "skip", traces = 1)
  }

#' get_values_for_boxes
#' @description get a list of summary stats for value boxes
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @param target_variable character, the name of the column to use for summary
#' @export
get_values_for_boxes <- function(observations, target_variable) {
  checkmate::assert_data_frame(observations)
  list(
    highest_temp = max(as.numeric(observations[[target_variable]]), na.rm = TRUE),
    lowest_temp  = min(as.numeric(observations[[target_variable]]), na.rm = TRUE),
    mean_temp    = mean(as.numeric(observations[[target_variable]]), na.rm = TRUE) %>% round(digits = 1)
  )
}
