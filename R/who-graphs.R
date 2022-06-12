#' who_threshold_data
#' @description Arrange the measurement according to WHO guidalines
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
who_threshold_data <- function(observations) {
  ## Arrange WHO Threshold Data
  observations %>%
    # only intereted in 11pm - 7am
    dplyr::filter(as.numeric(h) %in% c(23, 0:6)) %>%
    # threshold comparison
    dplyr::select(device_id, temp, name) %>%
    dplyr::mutate(
      sum_29   = dplyr::if_else(temp < 29 & temp >= 21, 1, 0),
      sum_21   = dplyr::if_else(temp < 21 & temp >= 18, 1, 0),
      # sum_20   = if_else(temp < 20 & temp >= 18, 1, 0),
      sum_18   = dplyr::if_else(temp < 18 & temp >= 16, 1, 0),
      sum_16   = dplyr::if_else(temp < 16 & temp >= 12, 1, 0),
      sum_12   = dplyr::if_else(temp < 12, 1, 0),
      sum_high = dplyr::if_else(temp >= 29, 1, 0)
    ) %>%
    # summarise by device_id
    dplyr::group_by(device_id, name) %>%
    dplyr::summarise(
      sum_high = sum(sum_high) / dplyr::n(),
      sum_29 = sum(sum_29) / dplyr::n(),
      sum_21 = sum(sum_21) / dplyr::n(),
      # sum_20 = sum(sum_20) / dplyr::n(),
      sum_18 = sum(sum_18) / dplyr::n(),
      sum_16 = sum(sum_16) / dplyr::n(),
      sum_12 = sum(sum_12) / dplyr::n()
    )
}

#' degrees_below_18_score
#' @description Calculate the degrees below 18C score
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
degrees_below_18_score <- function(observations) {
  # average degrees below 18
  browser()
  score <- observations %>%
    # only intereted in 11pm - 7am
    dplyr::filter(as.numeric(h) %in% c(23, 0:6)) %>%
    dplyr::mutate(cap_temp = ifelse(temp > 18, 18, temp)) %>%
    dplyr::summarise(sum(18 - cap_temp)) / nrow(.)

  # shitness score. 12deg (score = 6) -> 100.
  score <- 100 - 100 * (score / (18 - 12))
  floor(max(score, 0))
}


#' get_who_totals
#' @description Totals for Value Boxes
#' @param who_threshold_data data.frame, the output from `?who_threshold_data()`
#' @export
get_who_totals <- function(who_threshold_data) {
  who_totals <- who_threshold_data %>%
    dplyr::summarise_all(mean) %>%
    dplyr::summarise_at(c("sum_12", "sum_16", "sum_18", "sum_21", "sum_29", "sum_high"), mean)

  list(
    below_18 = who_totals$sum_18,
    below_16 = who_totals$sum_16,
    below_12 = who_totals$sum_12
  )
}


#' who_threshold_chart
#' @description WHO threshold chart
#' @param who_threshold_data data.frame, the output from `?who_threshold_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
who_threshold_chart <- function(who_threshold_data, from_timestamp, to_timestamp) {
  heatcols <-
    c("#F97162",
      "#FBE1B1",
      "#FEF9D7",
      "#B7DFCB",
      "#5ABAD1",
      "#3984B6")

  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)
  report_period <- report_period(from_timestamp , to_timestamp)
  who_chart <- who_threshold_data %>%
    dplyr::ungroup() %>%
    tidyr::gather("sum_temp", "hours", -c(device_id, name)) %>%
    dplyr::mutate(sum_temp = forcats::fct_rev(factor(sum_temp))) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = device_id,
      y = hours,
      fill = sum_temp
    )) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity",
      width = 0.6,
      colour = "#e9ecef"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.0), labels = scales::percent_format()) +
    ggplot2::scale_x_discrete(labels = who_threshold_data$name) +
    hrbrthemes::theme_ipsum() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 9,
        angle = 60,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = 9),
      plot.title  = ggplot2::element_text(size = 9),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title = "Night-time temperatures", title.position = "top"),
      drop = "none"
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = heatcols,
      labels = c(
        "Above 29°C",
        "21°C - 29°C",
        "Below 21°C",
        "Below 18°C",
        "Below 16°C",
        "Below 12°C"
      )
    ) +
    ggplot2::labs(title = paste0("Night time temperature ", report_period)) +
    ggplot2::xlab("") +
    ggplot2::ylab("of total night-time hours")

  plotly::ggplotly(who_chart)
}

#' who_guidance_kable
#' @description Render an html table of WHO guidelines
#' @export
who_guidance_kable <- function() {
  kableExtra::kable(
    tibble::tibble(
      indoor_temp = c(
        "above 29°C",
        "below 21°C",
        "below 18°C",
        "below 16°C",
        "below 12°C"
      ),
      effect = c(
        "Evidence from the modelling of outdoor temperature indicates <br>
                             that health effects are likely above 29 °C",
        "An indoor temperature of at least 21 °C is recommended for <br>
                             vulnerable groups including older people, children and those <br>
                             with chronic illnesses, particularly cardiorespiratory disease.",
        "18 °C is the WHO recommended minimum to provide a safe  <br>
                             and well-balanced indoor temperature to protect the health of  <br>
                             general populations during cold seasons. ",
        "Temperatures under 16 °C have been shown to have  <br>
                             cardiovascular effects.",
        "Temperatures under 12 °C have been shown to have  <br>
                             immediate reductions in children’s lung function. "
      )
    ),
    col.names = c("Indoor Temperature", "Health Effects"),
    # caption = "Table 2.1.1. Impact of Indoor temperature on health",
    escape = FALSE
  ) %>%
    kableExtra::kable_material(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      html_font = "sans-serif",
      full_width = TRUE
    ) %>%
    kableExtra::footnote(
      general = "Source WHO: Housing and Health Guidelines (ISBN: 978 92 4 155037 6)",
      general_title = "*",
      footnote_as_chunk = TRUE
    )
}

