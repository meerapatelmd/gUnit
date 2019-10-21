#' Reads Google Sheet as a list of dataframes or a single dataframe
#' @param gsheet_metadata gsheet_metadata
#' @import googlesheets
#' @import somersaulteR
#' @export
#'

read_gs <-
        function(gsheet_metadata, system.sleep = 2) {


                if (gsheet_metadata$n_ws > 1L) {
                        x <- list()
                        for (i in 1:gsheet_metadata$n_ws) {
                                x[[i]] <-
                                googlesheets::gs_read_csv(gsheet_metadata,
                                                          ws = i,
                                                          check.names = TRUE) %>%
                                        somersaulteR::call_mr_clean()
                                Sys.sleep(system.sleep)
                        }
                        names(x) <- tabnames_gs(gsheet_metadata)
                        return(x)
                } else if (gsheet_metadata$n_ws == 1L) {
                        x <-
                        googlesheets::gs_read_csv(gsheet_metadata,
                                                  ws = 1,
                                                  check.names = TRUE) %>%
                                somersaulteR::call_mr_clean()
                        return(x)
                }

        }
