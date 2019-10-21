#' Get colnames every tab in Google Sheet
#' @param gsheet_name name of Google Sheet
#' @import googlesheets
#' @export


retrieve_gs_colnames <-
        function(gsheet_name, system.sleep = 2) {
                gsheet_id <- retrieve_gsheet_id_by_name(gsheet_name)
                gs_ss <- googlesheets::gs_key(gsheet_id)
                x <- read_gs(gs_ss, system.sleep = system.sleep)
                x <- lapply(x, colnames)
                return(x)
        }
