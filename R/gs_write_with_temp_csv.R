#' Write to new Google Sheet
#' @param csv_filename full path to file to upload
#' @param new_gs_name name of new gs
#' @param system.sleep time in seconds for system sleep
#' @import dplyr
#' @importFrom googlesheets gs_upload
#' @export

gs_write_with_temp_csv <-
        function(data, new_gs_name, system.sleep = 0.3) {
                data_name <- deparse(substitute(data))
                if (is.data.frame(data)) {
                        data <- list(data_name = data)
                        names(data) <- data_name
                }

                gsheet_metadata_list <- list()
                for (i in 1:length(data)) {
                        dataframe <- data[[i]]
                        tmp_fn <- tempfile(fileext = ".csv")
                        readr::write_csv(dataframe, tmp_fn)
                        gsheet_metadata_list[[i]] <- googlesheets::gs_upload(file = tmp_fn,
                                                                   sheet_title = new_gs_name)
                        Sys.sleep(system.sleep)
                        names(gsheet_metadata_list)[i] <- names(data)[i]
                        unlink(tmp_fn)
                }
                return(gsheet_metadata_list)
        }
