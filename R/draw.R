draw_trend <- function(datetime, valve, STATION_NAME="", PERCENT_MAX=0.99, UNIT_NAME="psi") {
  SHADE_COLOR <- rgb(0.5, 0.5, 0.5, 0.75)
  QUANTILE_MIN <- 0.25
  QUANTILE_MAX <- 0.75

  #error check
  lubridate::is.POSIXt(datetime)
  is.numeric(valve)


  tmp <- data.frame(datetime, valve)


  ## TODO: Cleanup and remove redundant calls.
  if (PERCENT_MAX>=0) {
    data_hour <- tmp %>%
      dplyr::mutate(datehour=lubridate::round_date(datetime, "hour")) %>%
      dplyr::group_by(datehour) %>%
      dplyr::summarize(hour_valve=quantile(valve, probs=PERCENT_MAX, na.rm=TRUE)) %>%
      dplyr::mutate(date=lubridate::date(datehour)) %>%
      dplyr::mutate(wday=lubridate::wday(datehour)) %>%
      dplyr::mutate(hour=lubridate::hour(datehour))
  } else {
    data_hour <- tmp %>%
      dplyr::mutate(datehour=lubridate::round_date(datetime, "hour")) %>%
      dplyr::group_by(datehour) %>%
      dplyr::summarize(hour_valve=mean(valve, na.rm=TRUE)) %>%
      dplyr::mutate(date=lubridate::date(datehour)) %>%
      dplyr::mutate(wday=lubridate::wday(datehour)) %>%
      dplyr::mutate(hour=lubridate::hour(datehour))
  }


  data_diurnal <- data_hour %>%
    dplyr::group_by(wday, hour) %>%
    dplyr::summarize(
        mu_diurnal = mean(hour_valve, na.rm=TRUE)
      , q1_diurnal = quantile(hour_valve, na.rm=TRUE, probs=QUANTILE_MIN)
      , q3_diurnal = quantile(hour_valve, na.rm=TRUE, probs=QUANTILE_MAX)
    ) %>%
    dplyr::mutate(dt=wday+hour/24)

  data_daily <- data_hour %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(
      daily_valve = mean(hour_valve, na.rm=TRUE)
    )

  data_detrend <- data_hour %>%
    dplyr::mutate(dt=wday+hour/24) %>%
    merge(data_diurnal) %>%
    dplyr::mutate(detrend_hour_valve=hour_valve-mu_diurnal) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(
        mu_detrend = mean(detrend_hour_valve, na.rm=TRUE)
      , q1_detrend = quantile(detrend_hour_valve, na.rm=TRUE, probs=QUANTILE_MIN)
      , q3_detrend = quantile(detrend_hour_valve, na.rm=TRUE, probs=QUANTILE_MAX)
    )


  gg_diurnal <- data_diurnal %>%
    ggplot2::ggplot(aes(x=dt, y=mu_diurnal)) +
    ggplot2::geom_ribbon(
      aes(ymin=q1_diurnal, ymax=q3_diurnal)
      , fill=SHADE_COLOR
    ) +
    ggplot2::geom_line(lwd=1) +
    ggplot2::labs(
      x = "weekday"
      , y = paste0("diurnal (",UNIT_NAME, ")")
      , title = STATION_NAME
    ) +
    ggplot2::scale_x_continuous(
      breaks=1:7
      , minor_breaks = seq(1,8,0.25)
      , labels = c("S", "M", "T", "W", "T", "F", "S")
    )


  gg_freq <- data_hour %>%
    ggplot2::ggplot(aes(hour_valve)) +
    ggplot2::geom_freqpoly(lwd=1, bins=30) +
    ggplot2::labs(x = paste0("density (",UNIT_NAME,")"), y = "", title="") +
    ggplot2::theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

  gg_detrend <- data_detrend %>%
    ggplot2::ggplot(aes(x=date, y=mu_detrend)) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::geom_ribbon(
      aes(ymin=q1_detrend, ymax=q3_detrend)
      , fill = SHADE_COLOR
    ) +
    ggplot2::geom_line(lwd = 1) +
    ggplot2::scale_x_date(breaks="month",date_labels="%m/%y") +
    ggplot2::labs(x = "", y = paste0("trend (",UNIT_NAME,")"))


  gridExtra::grid.arrange(
    gg_diurnal, gg_freq, gg_detrend,
    widths = c(2, 1),
    layout_matrix = rbind(c(1, 2),
                          c(3, 3))
  )
}
