#' Adds a dataframe to a new tab with the given name to the gsheet identified by name
#' @import googlesheets
#' @import dplyr
#' @import typewriteR
#' @export

gs_add_tab <-
        function(dataframe, gsheet_name, tab_name, system.sleep = 3, log = TRUE, log_comment = "") {

                gs_ss <- gs_ss_by_name(gsheet_name)
                tab_names <- gs_tab_names(gs_ss)

                if (!(tab_name %in% tab_names)) {
                        gs_ss <- googlesheets::gs_ws_new(gs_ss, ws_title = tab_name, input = colnames(dataframe), byrow = TRUE)
                        gs_browse(gs_ss, ws = tab_name)
                        typewriteR::tell_me("Please add blank row to the new", tab_name,".")
                        typewriteR::stop_and_enter()
                }

                if (log == TRUE) {
                        project_log_timestamp <- mirroR::get_timestamp()
                }

                if (nrow(dataframe) > 0) {
                        for (i in 1:nrow(dataframe)) {
                                cat("\n")
                                typewriteR::tell_me(Sys.time(), "Starting row", i, "of total", nrow(dataframe), "rows.")
                                cat("\t\t")
                                gs_ss <- googlesheets::gs_add_row(gs_ss, ws = tab_name, input = dataframe %>%
                                                                                    dplyr::filter(row_number() == i))
                                cat("\n")
                                typewriteR::tell_me(Sys.time(), "Finished writing row", i, "of total", nrow(dataframe), "rows.")
                                cat("\n")
                                Sys.sleep(system.sleep)
                        }
                }

                if (log == TRUE) {
                        log_this_as(project_log_comment = log_comment,
                                    project_log_gsheet_title = gsheet_name,
                                    project_log_gsheet_tab_name = tab_name,
                                    project_log_timestamp = project_log_timestamp,
                                    project_log_write_timestamp = mirroR::get_timestamp())
                }
        }
