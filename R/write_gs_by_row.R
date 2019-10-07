#' Write to new Google Sheet
#' @param new_gs_name character string of the new Google Sheet name
#' @param data data to be written
#' @param rm_Sheet1 should Sheet1 be deleted?
#' @param system.sleep time in seconds for system sleep
#' @import googlesheets
#' @export

write_gs_by_row <-
        function(new_gs_name, data, rm_Sheet1 = TRUE, system.sleep = 0.3) {
                gsheet_metadata <<- googlesheets::gs_new(title = new_gs_name)

                if (is.data.frame(data)) {
                        data <- list(data)
                        names(data) <- deparse(substitute(data))
                        gsheet_metadata <- googlesheets::gs_ws_new(gsheet_metadata,
                                                input = colnames(data),
                                                ws_title = names(data))

                        Sys.sleep(system.sleep)
                }

                for (i in 1:length(data)) {
                                dataframe <<- data.frame(rbind(colnames(data[[i]]),
                                                       data[[i]] %>%
                                                               unname())
                                ) %>%
                                        unname()

                                row.names(dataframe) <- NULL

                                for (j in 1:nrow(dataframe)) {
                                        if (j %in% 1:5) {
                                                if (!(names(data)[i] %in% gsheet_metadata$ws$ws_title)) {
                                                        gsheet_metadata <- googlesheets::gs_ws_new(gsheet_metadata,
                                                                                                   ws_title = names(data)[i])
                                                        Sys.sleep(system.sleep)

                                                }


                                                gsheet_metadata <- googlesheets::gs_edit_cells(gsheet_metadata,
                                                                                           ws = names(data)[i],
                                                                                           input = dataframe[j,])

                                                Sys.sleep(system.sleep)
                                                readline(prompt = "Press [ENTER]")
                                        } else {
                                                gsheet_metadata <- googlesheets::gs_add_row(gsheet_metadata,
                                                                                            ws = names(data)[i],
                                                                                            input = dataframe[j,])
                                                Sys.sleep(system.sleep)
                                        }

                                }
                                Sys.sleep(system.sleep)
                }
                if (rm_Sheet1 == TRUE) {
                        gsheet_metadata <- googlesheets::gs_ws_delete(gsheet_metadata, ws = "Sheet1")
                }
                return(gsheet_metadata)
        }
