#' Write to new Google Sheet
#' @param csv_filename full path to file to upload
#' @param new_gs_name name of new gs
#' @param system.sleep time in seconds for system sleep
#' @importFrom googlesheets gs_upload
#' @export

gs_upload_csv <-
        function(csv_filename, new_gs_name, system.sleep = 0.3) {
                gsheet_metadata <- googlesheets::gs_upload(file = csv_filename,
                                        sheet_title = new_gs_name)
                Sys.sleep(system.sleep)
                return(gsheet_metadata$sheet_key)
        }
