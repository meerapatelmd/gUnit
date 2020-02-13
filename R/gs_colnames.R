#' Get colnames every tab in Google Sheet
#' @param gsheet_name name of Google Sheet
#' @import googlesheets
#' @export


gs_colnames <-
        function(gsheet_name, tab, system.sleep = 2) {
                gs_ss <- gs_ss_by_name(gsheet_name)
                x <- gs_read_tmp_csv(gsheet_name, tab = tab, system.sleep = system.sleep)
                if (is.list(x)) {
                        output <- lapply(x, colnames)
                } else if (is.data.frame(x)) {
                        output <- colnames(x)
                } else {
                        output <- NULL
                }

                return(output)
        }
