#' Write to new Google Sheet
#' @param new_gs_name character string of the new Google Sheet name
#' @param data data to be written
#' @param rm_Sheet1 should Sheet1 be deleted?
#' @importFrom googlesheets gs_new
#' @importFrom googlesheets gs_ws_new
#' @importFrom googlesheets gs_ws_delete
#' @export

write_gs <-
        function(new_gs_name, data, rm_Sheet1 = TRUE) {
                gsheet_metadata <- googlesheets::gs_new(title = new_gs_name)

                if (is.data.frame(data)) {
                        googlesheets::gs_ws_new(gsheet_metadata,
                                                input = data)
                } else if (is.list(data)) {
                        for (i in 1:length(data)) {
                                googlesheets::gs_ws_new(gsheet_metadata,
                                                        ws_title = names(data)[i],
                                                        input = data[[i]]
                                )
                        }
                }
                Sys.sleep(5)

                if (rm_Sheet1 == TRUE) {
                        googlesheets::gs_ws_delete(gsheet_metadata, ws = "Sheet1")
                }
                return(gsheet_metadata)
        }
