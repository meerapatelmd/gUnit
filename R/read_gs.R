#' Reads Google Sheet as a list of dataframes or a single dataframe
#' @param gsheet_name gsheet_name
#' @import googlesheets
#' @import somersaulteR
#' @export
#'

read_gs <-
        function(gsheet_name, tab = NULL, system.sleep = 3) {
                gsheet_metadata <- gs_ss_by_name(gsheet_name)
                Sys.sleep(system.sleep)
          if (is.null(tab)) {
                  if (gsheet_metadata$n_ws > 1L) {
                          x <- list()
                          for (i in 1:gsheet_metadata$n_ws) {
                                  x[[i]] <-
                                          googlesheets::gs_read_csv(gsheet_metadata,
                                                                    ws = i,
                                                                    check.names = TRUE,
                                                                    col_types = cols(.default = col_character())) %>%
                                          somersaulteR::call_mr_clean()
                                  Sys.sleep(system.sleep)
                          }
                          names(x) <- tabnames_gs(gsheet_metadata)
                          return(x)
                  } else if (gsheet_metadata$n_ws == 1L) {
                          x <-
                                  googlesheets::gs_read_csv(gsheet_metadata,
                                                            ws = 1,
                                                            check.names = TRUE,
                                                            col_types = cols(.default = col_character())) %>%
                                  somersaulteR::call_mr_clean()
                          return(x)
                  }
          } else {
                  x <- googlesheets::gs_read_csv(gsheet_metadata,
                                                 ws = tab,
                                                 check.names = TRUE,
                                                 col_types = cols(.default = col_character())) %>%
                         somersaulteR::call_mr_clean()
                  return(x)
          }


        }
