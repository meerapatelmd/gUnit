#' Gets the Google Sheet metadata
#' @param gsheet_id id
#' @importFrom googlesheets gs_key
#' @export
get_gsheet_metadata <-
        function(gsheet_id) {
                return(googlesheets::gs_key(gsheet_id))
        }
