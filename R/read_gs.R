#' Reads Google Sheet as a list of dataframes or a single dataframe
#' @param gsheet_metadata gsheet_metadata
#' @import googlesheets
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
                                                          check.names = TRUE,
                                                          col_types = cols(.default = col_character()))
                                Sys.sleep(system.sleep)
                        }
                        names(x) <- tabnames_gs(gsheet_metadata)
                        return(x)
                } else if (gsheet_metadata$n_ws == 1L) {
                        x <-
                        googlesheets::gs_read_csv(gsheet_metadata,
                                                  ws = 1,
                                                  check.names = TRUE,
                                                  col_types = cols(col_character())
                                                  )
                        return(x)
                }

        }
