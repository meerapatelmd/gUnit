#' Gets the Google Sheet metadata
#' @param gsheet_name name of the gsheet as an exact string match. An error will be thrown if there are more than 1 Google Sheets with that name.
#' @importFrom googlesheets gs_key
#' @export
#'
gs_ss_by_name <-
        function(gsheet_name) {
                gsheet_id <- get_gsheet_id_from_name(gsheet_name)
                return(googlesheets::gs_key(gsheet_id))
        }
