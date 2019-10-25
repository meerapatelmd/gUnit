#' Get Google Sheet id by name
#' @param gsheet_name character string of length one of the exact match
#' @import googledrive
#' @import typewriteR
#' @export


retrieve_gsheet_id_by_name <-
        function(gsheet_name) {
                dribble <- googledrive::drive_ls(pattern = paste0("^", gsheet_name, "$"),
                                      type = "spreadsheet")
                if (nrow(dribble) > 1) {
                        typewriteR::tell_me("ERROR:", "There are a total of", nrow(dribble), "Google Sheets with the name", gsheet_name, ".")
                } else if (nrow(dribble) == 0) {
                        return(NA)
                } else {
                        gsheet_id <- dribble$id
                        names(gsheet_id) <- gsheet_name
                        return(gsheet_id)
                }
        }
