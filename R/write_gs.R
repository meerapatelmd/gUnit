write_gs <-
        function(new_gs_name, data, rm_Sheet1 = TRUE) {
                gsheet_data <- googlesheets::gs_new(title = new_gs_name)
                
                if (is.data.frame(data)) {
                        googlesheets::gs_ws_new(gsheet_data,
                                                input = data)
                } else if (is.list(data)) {
                        for (i in 1:length(data)) {
                                googlesheets::gs_ws_new(gsheet_data,
                                                        ws_title = names(data)[i],
                                                        input = data[[i]]
                                )
                        }
                }
                Sys.sleep(10)
                return(gsheet_data)
        }