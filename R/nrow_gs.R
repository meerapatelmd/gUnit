#' Number of rows in each tab
#' @param gsheet_metadata gsheet_metadata
#' @export
nrow_gs <-
        function(gsheet_metadata) {
                gsheet_metadata$ws$row_extent
        }
