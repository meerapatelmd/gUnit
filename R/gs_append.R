#' Adds a dataframe to a Google Sheet tab by doing a dplyr::bind_row on the data downloaded as a tmp csv and completely rewriting the appended data to a new tab and deleting the old tab
#' @import typewriteR
#' @import dplyr
#' @import googlesheets
#' @export
#'
gs_append <-
        function(dataframe, gsheet_name, tab) {
                data <- gs_read_tmp_csv(gsheet_name, tab = tab)

                typewriteR::tell_me("There are", nrow(data), "rows in the original data.")
                cat("\n\n")
                df_to_add <- dataframe
                typewriteR::tell_me("There are", nrow(df_to_add), "rows in the new data.")
                cat("\n\n")

                typewriteR::tell_me("Columns in Google Sheet that are not in the input dataframe:\n\t", colnames(data)[!(colnames(data) %in% colnames(df_to_add))])
                cat("\n\n")
                typewriteR::tell_me("Columns in input dataframe that are not in the Google Sheet:\n\t", colnames(df_to_add)[!(colnames(df_to_add) %in% colnames(data))])
                cat("\n\n")

                final_data <-
                        dplyr::bind_rows(data,
                                  dataframe %>%
                                          somersaulteR::call_mr_clean()) %>%
                        dplyr::distinct()

                typewriteR::tell_me("There are", nrow(final_data), "rows in the new tab that will be written.")
                cat("\n\n")

                gs_ss <- mirCat::gs_ss_by_name(gsheet_name)
                gs_ss_01 <- try_catch_error_as_na(googlesheets::gs_ws_new(gs_ss, ws_title = paste0("NEW_", tab), input = colnames(final_data), byrow = TRUE))
                typewriteR::tell_me("Please add some blank spaces after the column names and come back and continue.")
                googlesheets::gs_browse(gs_ss_01, ws = paste0("NEW_", tab))
                typewriteR::stop_and_enter()

                for (i in 1:nrow(final_data)) {
                                cat("\n\n")
                                typewriteR::tell_me(Sys.time(), "Writing row number", i, "out of", nrow(final_data), "...")
                                cat("\n\n\t")
                                gs_ss_01 <- googlesheets::gs_add_row(gs_ss_01, ws = paste0("NEW_", tab), input = final_data %>%
                                                                             dplyr::filter(row_number() == i))
                                cat("\n\n")
                                typewriteR::tell_me(Sys.time(), "Finished Writing row number", i, "out of", nrow(final_data), "...")
                                cat("\n\n")
                                typewriteR::tell_me(Sys.time(), "System pause...")
                                cat("\n\n")
                                Sys.sleep(3)
                        }
                        if (!(is.vector(gs_ss_01))) {
                                gs_ss_02 <- try_catch_error_as_na(googlesheets::gs_ws_delete(gs_ss_01, ws = tab))
                                if (!(is.vector(gs_ss_02))) {
                                        cat("\n\n")
                                        googlesheets::gs_ws_rename(gs_ss_02, from = paste0("NEW_", tab), to = tab)
                                        cat("\n\n")
                                } else {
                                        cat("\n\n")
                                        typewriteR::tell_me("ERROR: Could not rename NEW_", tab, "to", tab, ".")
                                        cat("\n\n")
                                }
                        } else {
                                cat("\n\n")
                                typewriteR::tell_me("ERROR: Could not delete", tab, "to replace with NEW_", tab, ".")
                                cat("\n\n")
                        }

        }


