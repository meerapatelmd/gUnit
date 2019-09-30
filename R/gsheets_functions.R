source('~/R/gsheets_ss_functions.R')

get_gsheet_list <-
        function(name_pattern = NULL) {
                require(googledrive)
                if (is.null(name_pattern)) {
                        drive_find(type = "spreadsheet")
                } else {
                        drive_find(pattern = name_pattern, type = "spreadsheet")
                }
        }


##Related to actual Sheet
get_gsheet_tab_number <-
        function(gsheet_identifier) {
                ss <- get_gsheet_ss(gsheet_identifier)
                ss$n_ws
        }

get_gsheet_tab_names <-
        function(gsheet_identifier) {
                ss <- get_gsheet_ss(gsheet_identifier)
                ss$ws$ws_title
        }

get_gsheet_data <-
        function(gsheet_identifier,
                 tab, 
                 log = TRUE,
                 ...) {
                
                ss <- get_gsheet_ss(gsheet_identifier)
        
                if (log) {
                        desc <- readline("Describe this input. ")
                        write_log_entry(fn = ss$sheet_title,
                                        input_or_output = "input from Google Sheet",
                                        description = desc,
                                        google_id = ss$sheet_key
                        )
                }
                
                gs_read_csv(ss, ws = tab, ...) %>%
                        call_mr_clean()
        }

mv_gsheet <-
        function(gsheet_identifier, dest_gfolder_id) {
                require(googledrive)
                drive_mv(as_id(get_gsheet_key_from_gsheet_ss(get_gsheet_ss(gsheet_identifier))), path = as_id(dest_gfolder_id))
        }

view_gsheet_in_browser <-
        function(gsheet_identifier, tab = 1) {
                gs_browse(get_gsheet_ss(gsheet_identifier), ws = tab)
        }

write_new_gsheet <-
        function(dataframe = NULL,
                 gsheet_name,
                 tab_name = NULL,
                 dest_gfolder_id = NULL,
                 log = TRUE) {
                if (log == TRUE) {
                        desc <- readline("Describe this Google Sheet. ")
                        write_log_entry(fn = gsheet_name,
                                        input_or_output = "output to Google Sheet",
                                        description = desc,
                                        google_id = "")
                }
                        if (is.null(dataframe)) {
                                if (is.null(tab_name)) {
                                        ss <- gs_new(title = gsheet_name)
                                } else {
                                        ss <- gs_new(title = gsheet_name,
                                               ws_title = tab_name)
                                }
                        } else {
                                if (is.null(tab_name)) {
                                        ss <- gs_new(title = gsheet_name,
                                               input = dataframe)
                                } else {
                                        ss <- gs_new(title = gsheet_name,
                                               ws_title = tab_name,
                                               input = dataframe)
                                }
                        }
                
                if (!(is.null(dest_gfolder_id))) {
                        mv_gsheet(ss$sheet_key, dest_gfolder_id = dest_gfolder_id)
                }
                
                return(ss)
        }

add_tab_to_gsheet <-
        function(dataframe,
                 gsheet_identifier,
                 tab_name = NULL,
                 log = TRUE) {
                
                ss <- get_gsheet_ss(gsheet_identifier)
                
                if (log) {
                        desc <- readline("Describe this new Google Sheet tab. ")
                        write_log_entry(fn = ss$sheet_title,
                                        input_or_output = "output to Google Sheet",
                                        description = desc,
                                        google_id = ss$sheet_key)
                }
                
                if (!(is.null(tab_name))) {
                        gs_ws_new(ss, input = dataframe, ws_title = tab_name)
                } else {
                        gs_ws_new(ss, input = dataframe)
                }
        }




write_new_gsheets <-
        function(dataframe_list,
                 tab_names = NULL,
                 gsheet_name,
                 log = TRUE) {
                require(googledrive)

                if (is.null(tab_names)) {
                        tab_names <- names(dataframe_list)
                }
                
                stopifnot(is.list(dataframe_list))
                stopifnot(is.data.frame(dataframe_list[[1]]))
                stopifnot(!(is.null(tab_names)))
                
                dataframe <- dataframe_list[[1]]
                new_gsheet_ss <- write_new_gsheet(dataframe = dataframe, gsheet_name = gsheet_name, tab_name = tab_names[1])
                
                
                if (length(dataframe_list) > 1 & length(dataframe_list) == length(tab_names)) {
                        for (i in 2:length(dataframe_list)) {
                                add_tab_to_gsheet(dataframe_list[[i]],
                                                  gsheet_identifier = new_gsheet_ss$sheet_key,
                                                  tab_name =  tab_names[i],
                                                  log = log
                                )
                        }
                } else {
                        for (i in 2:length(dataframe_list)) {
                                tab_names <- paste0("Sheet", 2:length(dataframe_list))
                                add_tab_to_gsheet(dataframe_list[[i]],
                                                  gsheet_identifier = new_gsheet_ss$sheet_key,
                                                  tab_name = tab_names[i],
                                                  log = log
                                )
                        }
                        
                }
                
        }


upload_local_csv_as_gsheet <-
        function(full_fn,
                 gsheet_name,
                 dest_gfolder_id = NULL,
                 overwrite_google = TRUE,
                 remove_local = FALSE,
                 log = TRUE) {
                        require(googlesheets)
                        
                        if (overwrite_google) {
                                ss <- gs_upload(file = full_fn, sheet_title = gsheet_name, overwrite = TRUE)
                                if (!(is.null(dest_gfolder_id))) {
                                        require(googledrive)
                                        mv_gsheet(get_gsheet_key_from_gsheet_ss(ss), dest_gfolder_id = dest_gfolder_id)
                                }
                        } else {
                                require(googledrive)
                                duplicate_spreadsheets <- drive_ls(pattern = gsheet_name, type = "spreadsheet", n_max = 5)
                                if (nrow(duplicate_spreadsheets) == 0) {
                                        ss <- gs_upload(file = full_fn, sheet_title = gsheet_name, overwrite = TRUE)
                                        if (!(is.null(dest_gfolder_id))) {
                                                require(googledrive)
                                                mv_gsheet(get_gsheet_key_from_gsheet_ss(ss), dest_gfolder_id = dest_gfolder_id)
                                        }
                                } else if (nrow(duplicate_spreadsheets) == 1){
                                        tell_me("There is an existing sheet with the same title and they will all be deleted.")
                                        make_new_line()
                                        stop_and_enter()
                                        gs_delete(gs_key(duplicate_spreadsheets$id))
                                        ss <- gs_upload(file = full_fn, sheet_title = gsheet_name, overwrite = TRUE)
                                        if (!(is.null(dest_gfolder_id))) {
                                                require(googledrive)
                                                mv_gsheet(get_gsheet_key_from_gsheet_ss(ss), dest_gfolder_id = dest_gfolder_id)
                                        }
                                } else {
                                        tell_me("There are more than 1 Google Sheets with the same title. They will all be trashed and a single sheet will be overwritten.")
                                        make_new_line()
                                        stop_and_enter()
                                        sapply(1:length(duplicate_spreadsheets$id), function(i) gs_delete(gs_key(duplicate_spreadsheets$id[i])))
                                        ss <- gs_upload(file = full_fn, sheet_title = gsheet_name, overwrite = TRUE)
                                        if (!(is.null(dest_gfolder_id))) {
                                                require(googledrive)
                                                mv_gsheet(get_gsheet_key_from_gsheet_ss(ss), dest_gfolder_id = dest_gfolder_id)
                                        }
                                }
                        }
                
                if (remove_local) {
                        file.remove(full_fn)
                }
                
                if (log) {
                        desc <- readline("Describe the csv uploaded as Google Sheet. ")
                        write_log_entry(fn = ss$sheet_title,
                                        input_or_output = "output to Google Sheet",
                                        description = desc,
                                        google_id = ss$sheet_key)
                }
        }
 
##Delete Functions
delete_gsheets <-
        function(gsheet_identifiers) {
                gsheet_ss <- lapply(gsheet_identifiers, get_gsheet_ss)
                lapply(gsheet_ss, gs_delete)
        }


delete_gsheet_tab <-
        function(gsheet_identifier, tab_name, verbose = TRUE) {
                gsheet_ss <- get_gsheet_ss(gsheet_identifier)
                gs_ws_delete(gsheet_ss, ws = tab_name, verbose = verbose)
        }

##2 tabs in the same sheet are combined and one is deleted.
unite_gsheet_tabs <-
        function(gsheet_identifier, tab_1, tab_2, remove_tab_2 = TRUE) {
                TAB_1 <- gs_read_csv(get_gsheet_ss(gsheet_identifier), ws = tab_1) %>%
                        call_mr_clean()
                
                TAB_2 <- gs_read_csv(get_gsheet_ss(gsheet_identifier), ws = tab_2) %>%
                        call_mr_clean()
                
                gs_ws_delete(get_gsheet_ss(gsheet_identifier), ws = tab_1)
                
                gs_ws_new(get_gsheet_ss(gsheet_identifier), ws = tab_1, input = bind_rows(TAB_1, TAB_2))
                
                if (remove_tab_2) {
                        gs_ws_delete(get_gsheet_ss(gsheet_identifier), ws = tab_2)
                }
                
        }





