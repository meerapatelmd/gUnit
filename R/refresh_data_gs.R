#' Refreshes the object with the newest data in the gsheet
#' @param gsheet_id gsheet_id
#' @param robj_name name of object whose data needs to be refreshed
#' @export
refresh_data_gs <-
        function(gsheet_id, robj_name) {

                new_gsheet_metadata <- get_gsheet_metadata(gsheet_id)

                x <- read_gs(new_gsheet_metadata)

                assign(robj_name, x, envir = globalenv())
        }
