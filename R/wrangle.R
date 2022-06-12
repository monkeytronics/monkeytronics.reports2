#' Unix timestamp to NZ timezone datetime
#' @description Convert a time stamp into a NZ timezone datetime
#' @param timestamp vector of Unix time stamps
#' @return Returns "POSIXct", "POSIXt" vector in NZ time zone
#' @export
#' @examples
#' make_nz_datetime(1555555555)
make_nz_datetime <- function(timestamp) {
  checkmate::assert_numeric(timestamp)
  timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    lubridate::with_tz(tz = "Pacific/Auckland")
}

#' Print a formatted from-to sentence
#' @description Takes 2 time stapms and prints a formatted from-to sentence. Doesn't check if the 2nd timestamp is greater than the 1st one.
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
#' @return Character
#' @examples
#' report_period(1555555555, 1666666666)
#' @export
report_period <- function(from_timestamp, to_timestamp) {

  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)

  fromTime <- from_timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    format("%b %d")

  toTime <- to_timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    format('%b %d, %Y')

  return(paste("from", fromTime, "to", toTime))
}

#' wrangle_devices
#' @description wrangle the devices data
#' @param filtered_devices a data.frame or path to file
#' @importFrom readr col_character col_integer col_double
#' @examples
#' wrangle_devices(test_params()$filtered_devices)
#' @export
wrangle_devices <- function(filtered_devices,
                            data_type_filter = c("CD", "TH"),
                            na.strings = c("",NA),
                            integer_columns = c("floor"),
                            double_columns = c("long", "lat"),
                            character_columns = c("device_owner",
                                                  "device_id",
                                                  "data_type",
                                                  "comms_type",
                                                  "locationCode",
                                                  "addr",
                                                  "suburb",
                                                  "city",
                                                  "country",
                                                  "room",
                                                  "hhi",
                                                  "cluster",
                                                  "privacy",
                                                  "risk",
                                                  "buildingType",
                                                  "tenure",
                                                  "height",
                                                  "direction",
                                                  "school",
                                                  "building",
                                                  "classRoom",
                                                  "deployment_id")) {
  # checkmate::assert_class(x = filtered_devices, classes = c("data.frame", "character"))

  if (checkmate::test_character(filtered_devices)) {
    filtered_devices <-
      data.table::fread(
        filtered_devices,
        na.strings = na.strings,
        colClasses = list(character = character_columns,
                        numeric = double_columns,
                        integer = integer_columns)
        )
  }

  devices <-
    filtered_devices %>%
    # Only look at Sensor node devices
    # filter first mutate next
    dplyr::filter((data_type %in% data_type_filter)) %>%   # Sensor Node
    # Rental Type == 'council' / 'private'
    tibble::add_column(rental_type = "", .after = "cluster") %>%
    dplyr::mutate(rental_type = dplyr::case_when(
      grepl("council", tolower(cluster), fixed = TRUE) ~ "council",
      grepl("private", tolower(cluster), fixed = TRUE) ~ "private"
    )) %>%
    dplyr::mutate(rental_type = tidyr::replace_na(rental_type, "private")) %>%
    # Building Type == 'bunglaow', 'flat', 'house' ...
    tibble::add_column(building_type = "", .after = "cluster") %>%
    dplyr::mutate(building_type = dplyr::case_when(
      grepl("bungalow", tolower(cluster), fixed = TRUE) ~ "bungalow",
      grepl("flat", tolower(cluster), fixed = TRUE) ~ "flat",
      grepl("house", tolower(cluster), fixed = TRUE) ~ "house"
    )) %>%
    ## TODO: Check what to do with 'as.factor` calls in the case when mutate.
    ## Removed here as it appears they don't work with the test data
    dplyr::mutate(building_type = tidyr::replace_na(building_type, "unknown")) %>%

    ### Keeping this in case it becomes relevant at a later point
    # # tidy cluster string
    # mutate(cluster = str_replace_all(cluster, regex("\\W+"), "") %>%                         # remove all non AlphaNumeric Chars
    #                                  ifelse(. == "", "private_rental", .) %>%                # if cluster is empty, assume "private"
    #                                  replace_na(., "private_rental")) %>%                    # if cluster is NA, assume "private"

    # Mutate a room type for beroom / living space division
    tibble::add_column(room_type = "", .after = "room") %>%
    dplyr::mutate(room_type = dplyr::case_when(
      grepl("bedroom", tolower(room), fixed = TRUE) ~ "bedroom",
      grepl("lounge",  tolower(room), fixed = TRUE) ~ "living",
      grepl("living",  tolower(room), fixed = TRUE) ~ "living"
    )) %>%
    dplyr::mutate(room_type = tidyr::replace_na(room_type, "living")) %>%
    dplyr::mutate(addr = tidyr::replace_na(addr, "Unknown")) %>%
    dplyr::mutate(hhi = tidyr::replace_na(hhi, "Unknown"))

  ## If multiple rooms with same name, need unique name for graph axis, else use room names.
  if (length(unique(devices$room)) < length(devices$room)) {
    devices <- devices %>%
      dplyr::mutate(name = paste(device_id, ":", room))
  } else {
    devices <- devices %>%
      mutate(name = replace_na(room, "Unknown"))
  }
  return(devices)
}

#' wrangle_weather
#' @description wrangle the weather data
#' @param filtered_devices a data.frame or path to file
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
#' @examples
#' wrangle_weather(
#'   filtered_weather = test_params()$filtered_weather,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp
#' )
#' @export
wrangle_weather <- function(filtered_weather, from_timestamp, to_timestamp) {
  # checkmate::assert_choice(x = class(filtered_weather), choices = c("data.frame", "character"))
  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)

  from_time <- make_nz_datetime(from_timestamp)
  to_time <- make_nz_datetime(to_timestamp)

  if (checkmate::test_character(filtered_weather)) {
    weather <- readr::read_csv(filtered_weather, col_types = readr::cols())
  } else {
    weather <- filtered_weather
  }

  # add timezone to get NZ time
  weather %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dt_iso = (lubridate::ymd_hms(dt_iso) + as.numeric(timezone)) %>% lubridate::force_tz(tz = "Pacific/Auckland")
    ) %>%
    dplyr::mutate(city = as.character(city_name)) %>% ## prevents crash in hourly data, when empty data set
    dplyr::filter(!is.na(dt_iso)) %>%
    ## Hourly Outdoor Data
    # filter reporting period
    dplyr::filter(dt_iso %in% from_time:to_time) %>%

    # extract hour
    tibble::add_column(h = "", .after = "dt_iso") %>%
    dplyr::mutate(h = format(as.POSIXct(dt_iso, format = "%H:%M"), "%H")) %>%
    dplyr::mutate(outdoor_hum  = as.numeric(humidity)) %>%
    dplyr::mutate(outdoor_temp  = as.numeric(temp)) %>%

    # long_form
    dplyr::group_by(city, h) %>%
    # summarise( mean_t = mean(as.double(temp), na.rm = TRUE) ) %>%  # just temp
    dplyr::summarise_at(dplyr::vars("outdoor_hum", "outdoor_temp"), mean, na.rm = TRUE)    # add several values!
}

#' wrangle_observations
#' @description wrangle the measurements
#' @param filtered_obs a data.frame or path to file
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
#' @param devices a wrangled devices data frame as output by `wrangle_devices`
#' @param weather a wrangled outdoor weather data frame as output by `wrangle_weather`
#' @examples
#' wrangled_devices <- wrangle_devices(test_params()$filtered_devices)
#' wrangled_weather <- wrangle_weather(
#'   filtered_weather = test_params()$filtered_weather,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp
#' )
#' wrangle_observations(
#'   filtered_obs = test_params()$filtered_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp,
#'   devices = wrangled_devices,
#'   weather = wrangled_weather
#' )
#' @export
wrangle_observations <-
  function(filtered_obs,
           from_timestamp,
           to_timestamp,
           devices,
           weather) {
    # checkmate::assert_choice(x = class(filtered_obs),
    #                          choices = c("data.frame", "character"))
    checkmate::assert_number(from_timestamp)
    checkmate::assert_number(to_timestamp)
    checkmate::assert_data_frame(devices)

    from_time <- make_nz_datetime(from_timestamp)
    to_time <- make_nz_datetime(to_timestamp)

    if (checkmate::test_character(filtered_obs)) {
      obs <-
        readr::read_csv(filtered_obs, col_types = "ciiicd")
    } else {
      obs <- filtered_obs
    }

    obs %>%
      ## TODO: Should this become a flag, to keep only temp or not?
      # ## only need temp data for this report and all rows must have ts
      # filter(reading == "temp" & !is.na(ts)) %>%

      ## make sure to only include devices that are from the device array
      dplyr::filter(device_id %in% devices$device_id) %>%
      # make sure all ts is within the reporting period
      dplyr::filter(ts %in% from_time:to_time) %>%
      ## remove duplicate rows
      dplyr::distinct() %>%
      # device location
      dplyr::left_join(
        dplyr::select(
          devices,
          device_id,
          hhi,
          rental_type,
          room,
          name,
          room_type,
          city
        ),
        by = "device_id"
      ) %>%
      dplyr::relocate(hhi) %>%
      dplyr::relocate(c(rental_type, room), .before = "ts") %>%

      # datetime: nzst
      tibble::add_column(nzdt = "", .after = "ts") %>%
      dplyr::mutate(nzdt = make_nz_datetime(as.numeric(ts))) %>%
      dplyr::arrange(hhi, device_id, nzdt) %>%

      # extract hour
      tibble::add_column(h = "", .after = "nzdt") %>%
      dplyr::mutate(h = format(as.POSIXct(nzdt, format = "%H:%M:%S"), "%H")) %>%

      # Tidy Data
      dplyr::distinct(device_id, ts, seq, rssi, .keep_all = TRUE) %>%
      # tidyr::spread(key = reading, value = val) %>%

      # Join outdoor data : 'hourly_weather'
      dplyr::left_join(weather, by = c("h", "city"))
    # rename(outdoor_temp = temp) %>%
    # rename(outdoor_hum  = humidity)
  }

#' get_data_volume
#' @description Check data volume and ignore devices with < 10% data
#' @param observations data.frame, the output of `wrange_observations`
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
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
#' @export
get_data_volume <- function(observations, from_timestamp, to_timestamp) {

  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)
  checkmate::assert_data_frame(observations)

  from_time <- make_nz_datetime(from_timestamp)
  to_time <- make_nz_datetime(to_timestamp)

  # four observations per hour
  fullvol <- (to_time - from_time) %>% lubridate::as.duration() %>% as.numeric("hours") * 4

  # check data
  observations %>%
    dplyr::group_by(device_id) %>%
    dplyr::summarise(n = dplyr::n(),
              r = dplyr::n()/fullvol*100 %>% round(., digits = 1),
              end = max(nzdt),
              begin = min(nzdt),
              exclude = dplyr::if_else(r >= 10, 0, 1)) %>%
    dplyr::arrange(r)
}

#' get_devices_for_map
#' @description Filter the wrangled devices dataset to keep only devices with sufficient
#' volume for the map. This step has to go before exclusions so disconnected devices show.
#' @param devices a wrangled devices data frame as output by `wrangle_devices`
#' @param data_volume the data volume data frame as output by `get_data_volume`
#' @export
get_devices_for_map <- function(devices, data_volume) {

  checkmate::assert_data_frame(devices)
  checkmate::assert_data_frame(data_volume)

  devices %>%
    dplyr::mutate(connected = device_id %in% data_volume$device_id) %>%
    dplyr::select(c(device_id, long, lat, rental_type, connected, city)) %>%
    dplyr::filter(!is.na(long)) %>%
    dplyr::filter(!is.na(lat))
}

#' remove_excluded_devices
#' @description Remove rows from `target_data` if they are flagged to `exclude`
#' in the data_volume calculation
#' @param target_data data.frame, either the devices or observations tables
#' @param data_volume the data volume data frame as output by `get_data_volume`
#' @export
remove_excluded_devices <- function(target_data, data_volume) {
  checkmate::assert_data_frame(target_data)
  checkmate::assert_data_frame(data_volume)
  dplyr::filter(target_data, device_id %in% data_volume$device_id[data_volume$exclude == 0])
}

#' make_green_area
#' @description Make little Green area for plot
#' @param observations data.frame, the output of `wrange_observations`
#' @export
make_green_area <- function(observations) {

  checkmate::assert_data_frame(observations)

  # len <- length(observations)
  corner = c("bottom-left", "top-left", "top-right", "bottom-right")
  nzdt = c(min(observations$nzdt), min(observations$nzdt), max(observations$nzdt), max(observations$nzdt))
  temp = c(18, 29, 29, 18)
  hum = c(40, 70, 70, 40)
  co2 = c(400, 1000, 1000, 400)
  tibble::tibble(corner, nzdt, temp, hum, co2)
}

#' misc_values
#' @description calculate number of rows of some datasets
#' @param observations data.frame, the output of `wrange_observations`
#' @param devices a wrangled devices data frame as output by `wrangle_devices`
#' @export
nrow_values <- function(devices, observations) {

  checkmate::assert_data_frame(devices)
  checkmate::assert_data_frame(observations)

  alldevice_n <- nrow(devices)
  # Calculate final numbers after removing excluded
  included_n <- nrow(devices)
  excluded_n <- alldevice_n - included_n
  # hhi_count <- n_distinct(devices$hhi, na.rm = TRUE) # update based only on included devices
  devices_rp <- observations %>% dplyr::distinct(device_id)

  list(
    alldevice_n = alldevice_n,
    included_n = included_n,
    excluded_n = excluded_n,
    devices_rp = devices_rp
  )
}
