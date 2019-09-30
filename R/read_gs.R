read_gs <-
        function(gsheet_data) {
                
                if (gsheet_data$n_ws > 1L) {
                        x <- list()
                        for (i in 1:gsheet_data$n_ws) {
                                x[[i]] <-
                                googlesheets::gs_read_csv(gsheet_data,
                                                          ws = i,
                                                          check.names = TRUE,
                                                          col_types = paste(rep("c", ncol_gs(gsheet_data)[i]), collapse = ""))
                                Sys.sleep(10)
                        }
                        names(x) <- tabnames_gs(gsheet_data)
                        return(x)
                } else if (gsheet_data$n_ws == 1L) {
                        x <-
                        googlesheets::gs_read_csv(gsheet_data,
                                                  ws = 1,
                                                  check.names = TRUE,
                                                  col_types = paste(rep("c", ncol_gs(gsheet_data)[1]), collapse = "")
                                                  )
                        return(x)
                }
                
        }