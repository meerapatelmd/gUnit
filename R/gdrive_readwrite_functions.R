source('~/R/gdrive_functions.R')
parse_google_timestamp <-
        function(gtimestamp) {
                require(lubridate)
                parse_date_time(gtimestamp, order = "%a, %d %b %Y %H:%M:%S %z")
        }

write_to_gdrive <-
        function(gfolder_id, local_file_path, overwrite_gdrive = FALSE, verbose = FALSE, log = TRUE) {
                require(googledrive)
                fn <- basename(local_file_path)
                if (overwrite_gdrive == FALSE) {
                        if (file_exists_in_gfolder(gfolder_id = gfolder_id, filename = local_file_path) == TRUE) {
                                tell_me(paste(fn, "already exists in the", get_gname_from_id(gfolder_id), "folder and another copy is being made.", sep = " ", collapse = " "))
                                dribble <- drive_upload(local_file_path, path = as_id(gfolder_id), verbose = verbose)
                        } else {
                                dribble <- drive_upload(local_file_path, path = as_id(gfolder_id), verbose = verbose)
                        }
                } else {
                        if (get_gfolder_file_number(gfolder_id = gfolder_id) > 1) {
                                tell_me(paste("More than one files with the name", fn, "already exists in the", get_gname_from_id(gfolder_id), "folder and they will all be overwritten:", sep = " ", collapse = " "))
                                gfiles_to_overwrite <- drive_ls(path = as_id(gfolder_id), pattern = fn)
                                print(gfiles_to_overwrite)
                                stop_and_enter()
                                drive_trash(as_dribble(gfiles_to_overwrite), verbose = verbose)
                                dribble <- drive_upload(local_file_path, path = as_id(gfolder_id), verbose = verbose)
                        } else if (number_files_in_gfolder(gfolder_id = gfolder_id, local_file_path = local_file_path, max = 10) == 1) {
                                tell_me(paste(fn, "already exists in the", get_gname_from_id(gfolder_id), "folder and it will be overwritten:", sep = " ", collapse = " "))
                                gfiles_to_overwrite <- drive_ls(path = as_id(gfolder_id), pattern = fn)
                                print(gfiles_to_overwrite)
                                stop_and_enter()
                                drive_trash(as_dribble(gfiles_to_overwrite), verbose = verbose)
                                dribble <- drive_upload(local_file_path, path = as_id(gfolder_id), verbose = verbose)
                        } else {
                                dribble <- drive_upload(local_file_path, path = as_id(gfolder_id), verbose = verbose)
                        }
                }
                
                if (log == TRUE) {
                        source('~/R/log_functions.R')
                        tell_me(local_file_path)
                        desc <- readline("Describe the file that was just written to Google Drive. ")
                        write_log_entry(fn = fn,
                                        input_or_output = "to Google Drive",
                                        description = desc, 
                                        google_id = get_ids_from_dribble(dribble))
                }
        }








