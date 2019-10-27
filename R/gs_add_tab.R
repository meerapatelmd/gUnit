#' Adds a dataframe to a new tab with the given name to the gsheet identified by name
#' @import googlesheets
#' @import dplyr
#' @import typewriteR
#' @export

gs_add_tab <-
        function(dataframe, gsheet_name, tab_name) {
                gsheet_id <- get_gsheet_id_from_name(gsheet_name)
                gsheet_metadata <- googlesheets::gs_key(gsheet_id)

                tab_names <- gs_tabnames(gsheet_metadata)

                if (!(tab_name %in% tab_names)) {
                        gsheet_metadata <- googlesheets::gs_ws_new(googlesheets::gs_key(gsheet_id), ws_title = tab_name, input = colnames(dataframe), byrow = TRUE)
                        gs_browse(gs_ss, ws = tab_name)
                        typewriteR::tell_me("Please add blank row to the new", tab_name, "and")
                        typewriteR::stop_and_enter()
                }

                for (i in 1:nrow(dataframe)) {
                        cat("\n")
                        typewriteR::tell_me(Sys.time(), "Starting row", i, "of total", nrow(dataframe), "rows.")
                        cat("\t\t")
                        gs_ss <- googlesheets::gs_add_row(gs_ss, ws = tab_name, input = dataframe %>%
                                                                                                dplyr::filter(row_number() == i))
                        cat("\n")
                        typewriteR::tell_me(Sys.time(), "Finished writing row", i, "of total", nrow(dataframe), "rows.")
                        cat("\n")
                        Sys.sleep(4)
                }
        }
