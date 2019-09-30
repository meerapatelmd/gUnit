#' Get number of cols in each tab
#' @param gsheet_metadata object derived from the
#' @export
ncol_gs <-
        function(gsheet_metadata) {
                gsheet_metadata$ws$col_extent
        }
