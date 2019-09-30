#' Reads Google Sheet as a list of dataframes or a single dataframe
#' @param gsheet_metadata gsheet_metadata
#' @importFrom googlesheets gs_read_csv
#' @export
read_gs <-
        function(gsheet_metadata) {

                if (gsheet_metadata$n_ws > 1L) {
                        x <- list()
                        for (i in 1:gsheet_metadata$n_ws) {
                                x[[i]] <-
                                googlesheets::gs_read_csv(gsheet_metadata,
                                                          ws = i,
                                                          check.names = TRUE,
                                                          col_types = paste(rep("c", ncol_gs(gsheet_metadata)[i]), collapse = ""))
                                Sys.sleep(5)
                        }
                        names(x) <- tabnames_gs(gsheet_metadata)
                        return(x)
                } else if (gsheet_metadata$n_ws == 1L) {
                        x <-
                        googlesheets::gs_read_csv(gsheet_metadata,
                                                  ws = 1,
                                                  check.names = TRUE,
                                                  col_types = paste(rep("c", ncol_gs(gsheet_metadata)[1]), collapse = "")
                                                  )
                        return(x)
                }

        }
