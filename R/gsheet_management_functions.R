
dedupe_gsheet <-
        function(gsheet_id, tab_name = "Sheet1", .keep_all = TRUE, log = TRUE) {
                gsheet_data <- get_gsheet_data(gsheet_identifier = gsheet_id, tab_name = tab_name, log = FALSE)
                
                gs_ws_delete(get_gsheet_ss(gsheet_id), ws = tab_name)
                
                gs_ws_new(get_gsheet_ss(gsheet_id),
                          ws_title = tab_name,
                          input = distinct(gsheet_data, .keep_all = .keep_all))
                
                if (log) {
                        write_log_entry(fn = get_gsheet_name_from_id(gsheet_id),
                                        input_or_output = "output",
                                        description = "deduplicated the data",
                                        google_id = gsheet_id
                        )
                }
        }
