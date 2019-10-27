#' Reads Google Sheet as a list of dataframes or a single dataframe
#' @param gsheet_name gsheet_name
#' @import googlesheets
#' @importFrom readr read_csv
#' @import somersaulteR
#' @export
#'

read_gs_by_tmp_csv <-
        function(gsheet_name, tab = NULL, system.sleep = 3) {
                gsheet_metadata <- gs_ss_by_name(gsheet_name)
                Sys.sleep(system.sleep)
          if (is.null(tab)) {
                  if (gsheet_metadata$n_ws > 1L) {
                          x <- list()
                          for (i in 1:gsheet_metadata$n_ws) {
                                  tmp_csv_fn <- tempfile(fileext = ".csv")
                                          googlesheets::gs_download(from = gsheet_metadata,
                                                                    ws = i,
                                                                    to = tmp_csv_fn)
                                          x[[i]] <- readr::read_csv(file = tmp_csv_fn,
                                                                    col_types = cols(.default = "c"))
                                  Sys.sleep(system.sleep)
                                  unlink(tmp_csv_fn)
                          }
                          names(x) <- tabnames_gs(gsheet_metadata)
                          return(x)
                  } else if (gsheet_metadata$n_ws == 1L) {
                          tmp_csv_fn <- tempfile(fileext = ".csv")
                          googlesheets::gs_download(from = gsheet_metadata,
                                                    ws = i,
                                                    to = tmp_csv_fn)
                          x <- readr::read_csv(file = tmp_csv_fn,
                                                    col_types = cols(.default = "c"))
                          Sys.sleep(system.sleep)
                          unlink(tmp_csv_fn)
                          return(x)
                  }
          } else {
                  tmp_csv_fn <- tempfile(fileext = ".csv")
                  googlesheets::gs_download(from = gsheet_metadata,
                                            ws = tab,
                                            to = tmp_csv_fn)
                  x <- readr::read_csv(file = tmp_csv_fn,
                                       col_types = cols(.default = "c"))
                  Sys.sleep(system.sleep)
                  unlink(tmp_csv_fn)
                  return(x)
          }
        }
