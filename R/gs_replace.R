#' Replaces a tab with new dataframe
#' @import googlesheets
#' @export

gs_replace <-
        function(dataframe, gsheet_name, tab_name) {
                gsheet_id <- get_gsheet_id_from_name(gsheet_name)
                gsheet_metadata <- googlesheets::gs_key(gsheet_id)
                googlesheets::gs_ws_rename(gsheet_metadata, from = tab_name, to = "OLD")
                gs_add_tab(dataframe, gsheet_name = gsheet_name, tab_name = tab_name)
                googlesheets::gs_ws_delete(gsheet_metadata, ws = "OLD")
        }
