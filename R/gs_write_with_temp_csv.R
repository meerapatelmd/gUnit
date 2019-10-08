#' Write to new Google Sheet
#' @param csv_filename full path to file to upload
#' @param new_gs_name name of new gs
#' @param system.sleep time in seconds for system sleep
#' @importFrom googlesheets gs_upload
#' @export

gs_write_with_temp_csv <-
        function(data, new_gs_name, system.sleep = 0.3) {
                data_name <- deparse(substitute(data))
                if (is.data.frame(data)) {
                        data <- list(data_name = data)
                        names(data) <- data_name
                }

                for (i in 1:length(data)) {
                        dataframe <- data[[i]]
                }
                gsheet_metadata <- googlesheets::gs_upload(file = csv_filename,
                                        sheet_title = new_gs_name)
                Sys.sleep(system.sleep)
                return(gsheet_metadata$sheet_key)
        }
