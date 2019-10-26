#' Gets the Google Sheet metadata
#' @param gsheet_id id
#' @importFrom googlesheets gs_key
#' @export
#'
gs_m <-
        function(gsheet_id) {
                return(googlesheets::gs_key(gsheet_id))
        }
