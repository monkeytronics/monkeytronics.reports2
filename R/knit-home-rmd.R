#' Param list
#' @description Create a list of parameters for knitting rmd
#' @param .list A named list of key-value pairs
#' @param ... Unquoted key-value pairs
#' @details Field options are:
#'   filtered_obs,
#'   filtered_weather,
#'   filtered_devices,
#'   filtered_interventions,
#'   report_params,
#'   fromTimeStamp,
#'   toTimeStamp
#'
#' @examples
#' # convenient for interactive typing
#' create_param_list(filtered_obs = "inst/data/filtered_obs.csv")
#' # convenient for programming
#' create_param_list(.list = list(filtered_obs = "inst/data/filtered_obs.csv"))
#' @export
make_params_list <- function(.list = NULL, ...) {
  fields <- c(
    "filtered_obs",
    "filtered_weather",
    "filtered_devices",
    "filtered_interventions",
    "report_params",
    "fromTimeStamp",
    "toTimeStamp"
  )
  lapply(names(.list), checkmate::assert_choice, unlist(fields))
  lapply(names(list(...)), checkmate::assert_choice, unlist(fields))
  c(.list, list(...))
}

#' Compile the home report
#'
#' @description Create a the home report
#' @param param_list named list of parameters, see `?make_param_list`
#' @param output_dir path to folder to save the html (default: current work dir)
#' @param output_file name of the output html (default: home-{timestamp}.html)
#' @param ... additional named arguments passed onto `rmarkdown::render`
#'
#' @examples
#' obs  <- system.file("data/filtered_obs.csv", package="monkeytronics.reports")
#' dev  <- system.file("data/filtered_devices.csv", package="monkeytronics.reports")
#' wea  <- system.file("data/filtered_weather.csv", package="monkeytronics.reports")
#' int  <- system.file("data/filtered_interventions.csv", package="monkeytronics.reports")
#' from <- 1627639151
#' to   <- 1630317551
#'
#' par_list <- make_params_list(
#'   filtered_obs = obs,
#'   filtered_devices = dev,
#'   filtered_weather = wea,
#'   filtered_interventions = int,
#'   fromTimeStamp = from,
#'   toTimeStamp = to
#' )
#' knit_home_report(par_list)
#'
#' \dontrun{
#' # using the database
#' # assumes DB credentials are set as environmental variables
#' # see `?dynamo_connect`
#' dev <- dynamo_query(
#' .con = dynamo_connect(),
#' .table = "sn-v1-devices",
#' .partition_key = "device_owner",
#' .partition_value = "al@monkeytronics.co.nz"
#' )
#'
#' int <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "sn-v1-interventions",
#'   .partition_key = "device_id",
#'   .partition_value = "W000011"
#' )
#'
#' wea <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "sn-v1-weather-db",
#'   .partition_key = "city",
#'   .partition_value = "Wellington"
#' )
#'
#' obs <- dynamo_query(
#'   .con = dynamo_connect(),
#'   .table = "SensorNodeData",
#'   .partition_key = "device_id",
#'   .partition_value = "W000011"
#' )
#' from <- 1627639151
#' to   <- 1630317551
#'
#' par_list <- make_params_list(
#'   filtered_obs = obs,
#'   filtered_devices = dev,
#'   filtered_weather = wea,
#'   filtered_interventions = int,
#'   fromTimeStamp = from,
#'   toTimeStamp = to
#' )
#' knit_home_report(par_list)
#'
#' }
#'
#' @export
knit_home_report <-
  function(param_list,
           output_file = paste0("home-", as.numeric(Sys.time()), ".html"),
           output_dir = getwd(),
           ...
           ) {
    ## Get directory of report markdown template
    report_rmd <-
      system.file("home.rmd", package = "monkeytronics.reports")

    ## Render report into html
    rmarkdown::render(
      input = report_rmd,
      params = param_list,
      output_file = output_file,
      output_dir = output_dir,
      ...
    )
  }
