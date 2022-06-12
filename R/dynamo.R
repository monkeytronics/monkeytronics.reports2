#' dynamo_connect
#' @description connect to an AWS DynamoDB given credentials stored in `.Renviron`.
#' Credentials are fetched with `Sys.getenv`. Required entries in the `.Renviron` are:
#'   `ACCESS_KEY_ID`, `SECRET_ACCESS_KEY`, `PROFILE`, and `REGION`
#'
#' @examples
#' con <- dynamo_connect()
#' @export
dynamo_connect <- function() {
  paws::dynamodb(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("ACCESS_KEY_ID"),
          secret_access_key = Sys.getenv("SECRET_ACCESS_KEY")
        ),
        profile = Sys.getenv("PROFILE")
      ),
      region = Sys.getenv("REGION")
    )
  )
}

#' dynamo_bulk_put
#' @description Load a data frame to AWS DynamoDB. Items with matching partition key
#' will be overwritten
#' @param .con a connection to a DynamoDB database (see ?paws::dynamodb)
#' @param .table the name of the table to append or replace rows
#' @param .df a data frame we wish to load
#'
#' @examples
#' # prepare a version of iris without factors and an ID column
#' # to match the partition key in the remote table
#' iris_to_put <- iris
#' iris_to_put$Species <- as.character(iris_to_put$Species)
#' iris_to_put$ID <- 300:151
#' \dontrun{
#' con <- dynamo_connect()
#' dynamo_bulk_put(.con = con, .table = "Iris", .df = iris_to_put)
#' }
#' @export

dynamo_bulk_put <- function(.con, .table, .df, .list = NULL) {
  if (!is.null(.list)) {
    requests <-
      dynamo_df_prep_list(.df = .df,
                          .list_name = .list[[1]],
                          .list_value = .list[[2]])
  } else {
    requests <- lapply(1:nrow(.df), function(i) {
      list(PutRequest = list(Item = dynamo_item_prep(.item = .df[i,])))
    })
  }

  n_items <- length(requests)
  # from https://stackoverflow.com/a/18857482/8543257
  chunked <-
    split(requests, rep(1:ceiling(n_items / 25), each = 25)[1:n_items])

  lapply(chunked, function(L) {
    requestList <- list()
    requestList[[.table]] <- L
    .con$batch_write_item(RequestItems = requestList)
  })
}

#' unexported helpers
#' @noRd

guess_attrib <- function(types) {
  # add more as needed
  switch(types,
         "numeric" = "N",
         "integer" = "N",
         "character" = "S",
         "logical" = "BOOL")
}

#' @noRd
dynamo_item_prep <- function(.item) {
  types <- lapply(.item, class)
  attribs <- lapply(types, guess_attrib)
  nested <- lapply(seq_along(.item), function(i) as.list(setNames(.item[[i]], attribs[[i]])))
  setNames(nested, names(.item)) %>%
    purrr::discard(is.na) %>%
    purrr::discard(is.null)
}

#' @noRd
prep_list <- function(.item, .list) {
  list_bit <- list()
  list_bit[["L"]] <- list()
  # TODO: check as early as possible if missing or null
  # to avoid needing to drop it from above later
  type <- guess_attrib(class(unlist(.item[[.list]])))
  list_bit[["L"]][["val"]] <- as.list(rlang::set_names(unlist(.item[[.list]]), type))
  list_bit
}

#' @noRd
# TODO: this function can probably be removed. its not used currently
dynamo_item_prep_list <- function(.item, .list_name, .list_value) {
  list_bit <- list()
  list_bit[["L"]] <- list()
  list_bit[["L"]][["val"]] <- as.list(setNames(.item[[.list_value]], guess_attrib(class(.item[[.list_value]]))))

  flat_bit <- purrr::discard(.item, names(.item) %in% c(.list_name, .list_value)) %>% as.list()
  flat_prep <- dynamo_item_prep(flat_bit)
  flat_prep[[ .item[[.list_name]] ]] <- list_bit
  flat_prep
}

#' @noRd
dynamo_df_prep_list <-
  function(.df,
           # .partition_key,
           # .sort_key,
           .list_name,
           .list_value) {
    # TODO: remove unused code unless established otherwise
    # .pkey <- rlang::sym(.partition_key)
    # .skey <- rlang::sym(.sort_key)

    group_vars <- purrr::discard(
      .x = names(.df),
      .p = names(.df) %in% c(.list_name, .list_value)
      ) %>% rlang::syms()

    rshp <-
      .df %>%
      dplyr::group_by(!!!group_vars) %>%
      tidyr::pivot_wider(names_from = .list_name, values_from = .list_value, values_fn = list)

    # cycle ver rows and form the items
      lapply(1:nrow(rshp), function(i) {

      # form lists for reshaped row
      .lists <- unique(.df[[.list_name]])
      lists_prep <- lapply(.lists, prep_list, .item = rshp[i, ]) %>%
        setNames(.lists) %>%
        purrr::discard(.p = ~ all(is.null(.[["L"]][["val"]][[1]]))) %>%
        purrr::discard(.p = ~ all(is.na(.[["L"]][["val"]][[1]])))

      # form flat bits for reshaped row
      flat_bit <- purrr::discard(dplyr::ungroup(rshp[i, ]), names(rshp[i, ]) %in% .lists) %>% as.list()
      flat_prep <- dynamo_item_prep(.item = flat_bit) %>%
        purrr::discard(is.na) %>%
        purrr::discard(is.null)
      # add lists to flat bit
      # adorn for dynamo batch write
      list(PutRequest = list(Item = c(flat_prep, lists_prep)))
      # %>% purrr::modify_depth(.depth = 3, .f = unlist)
    })
  }

#' @noRd
dynamo_item_put <- function(.con, .table, .prep) {
  .con$put_item(
    TableName = .table,
    Item = .prep
  )
}

