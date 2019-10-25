#' Updates a the list of Google Sheets metadata by the name of the Google Sheet
#' @param gs_ss_list list of Google Sheets metadata named by the Google Sheet name
#' @param gsheet_name name of the Google Sheet where the ss needs to be updated
#' @import googlesheets
#' @export

update_gs_ss_by_name <-
        function(gs_ss_list, gsheet_name) {
                gsheet_id <- get_gsheet_id_from_name(gsheet_name)
                gs_ss_list[[gsheet_name]] <- googlesheets::gs_key(gsheet_id)
                return(gs_ss_list)
        }
