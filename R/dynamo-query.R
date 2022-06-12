#' dynamo_query
#' @description Get data from table with a partition key. Not used.
#'
#' @param .con a connection to a DynamoDB database (see ?paws::dynamodb)
#' @param .table the name of the table to append or replace rows
#' @param .partition_key the name of the `partition key` field
#' @param .partition_value the value of the `partition key`
#'
#' @examples
#' dynamo_query(
#' .con = dynamo_connect(),
#' .table = "sn-v1-devices",
#' .partition_key = "device_owner",
#' .partition_value = "al@monkeytronics.co.nz"
#' )
#' dynamo_query(
#' .con = dynamo_connect(),
#' .table = "sn-v1-interventions",
#' .partition_key = "device_id",
#' .partition_value = "W000011"
#' )
#' dynamo_query(
#' .con = dynamo_connect(),
#' .table = "sn-v1-weather-db",
#' .partition_key = "city",
#' .partition_value = "Wellington"
#' )
#' dynamo_query(
#' .con = dynamo_connect(),
#' .table = "SensorNodeData",
#' .partition_key = "device_id",
#' .partition_value = "W000011"
#' )
#' @export
dynamo_query <-
  function(.con,
           .table,
           .partition_key,
           .partition_value) {
    Q <- .con$query(
      ExpressionAttributeValues = list(`:pkey` = list(S = .partition_value)),
      KeyConditionExpression = paste(.partition_key,  "= :pkey"),
      TableName = .table
    )$Items

    purrr::map_dfr(Q, function(xx) {
      purrr::map_chr(xx, function(aa) {
        purrr::keep(aa, ~ length(.) > 0)[[1]] %>% unlist()
      })
    })
  }

