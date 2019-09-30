source('~/R/gdrive_functions.R')
source('~/R/gdrive_readwrite_functions.R')

is_gdoc_fn <-
        function(filename) {
                grepl("[.]{1}gdoc$", filename)
        }

convert_gdoc_to_pdf <-
        function(gdoc_id,
                 local_gdrive_path,
                 g_overwrite = FALSE) {
                dribble <- drive_download(as_id(gdoc_id),
                               type = "pdf",
                               overwrite = TRUE)
                fn <- get_names_from_dribble(dribble)
                fn <- paste0(fn, ".pdf")
                print(paste0(local_gdrive_path, "/", fn))
                stop_and_enter()
                file.rename(from = fn,
                            to   = paste0(local_gdrive_path, "/", fn))
                
                
        }


transform_gfolder_gdocs_to_pdf <-
        function(gfolder_id, recursive = FALSE, 
                 local_folder = "INPUT", sub_folder = "", overwrite_local = FALSE, 
                 overwrite_gdrive = FALSE,
                 delete_gdocs = FALSE,
                 delete_local_pdf = TRUE,
                 log = FALSE) {
                dribble <- dl_gdoc_dribble_as_pdf(get_gdocs_in_gfolder_dribble(gfolder_id, recursive = recursive), local_folder = local_folder, sub_folder = sub_folder, overwrite = overwrite_local)
                dribble <- bind_rows(dribble)
                lapply(1:nrow(legend), function(i) write_to_gdrive(gfolder_id, legend$local_path[i], overwrite_gdrive = overwrite_gdrive, log = log))
                if (delete_local_pdf) {
                        sapply(dribble$local_path, file.remove)
                        if (length(list.files(paste0(local_folder, "/", sub_folder))) == 0) {
                                unlink(paste0(local_folder, "/", sub_folder), recursive = TRUE)
                        }
                }
                
                if (delete_gdocs) {
                        google_ids <- get_ids_from_dribble(dribble)
                        lapply(1:length(google_ids), function(i) drive_trash(as_id(google_ids[i])))
                }
        }


get_gdocs_in_gfolder_dribble <-
        function(gfolder_id, recursive = FALSE) {
                require(googledrive)
                drive_ls(path = as_id(gfolder_id), type = "document", recursive = recursive)
        }

dl_gdoc_dribble_as_pdf <-
        function(dribble, local_folder = "INPUT", sub_folder = "", overwrite = TRUE) {
                base_fns <- rename_vector_duplicates(get_names_from_dribble(dribble))
                if (sub_folder == "") {
                        full_fns <- paste0("./", local_folder, "/",  base_fns)
                } else {
                        local_path <- paste0("./", local_folder, "/", sub_folder)
                        if (!(dir.exists(local_path))) {
                                dir.create(local_path)
                        }
                        full_fns <- paste0(local_path, "/",  base_fns)
                }
                
                google_ids <- get_ids_from_dribble(dribble)
                
                lapply(1:length(google_ids), function(i) drive_download(file = as_id(google_ids[i]), path = full_fns[i], type = "pdf", overwrite = overwrite))
                
        }


dl_gdoc_as_pdf <-
        function(gdoc_id, remove_gdrive = FALSE, overwrite_local = TRUE, log = TRUE) {
                require(googledrive)
                if (remove_gdrive == TRUE) {
                        drive_download(as_id(gdoc_id), type = "pdf", overwrite = overwrite_local)
                        if (log == TRUE) {
                                source('~/R/log_functions.R')
                                tell_me(get_gname_from_id(gdoc_id))
                                desc <- readline("Describe the file that was just downloaded from Google Drive. ")
                                write_log_entry(fn = fn,
                                                input_or_output = "from Google Drive",
                                                description = desc)
                        }
                        drive_trash(as_id(gdoc_id)) 
                } else {
                        drive_download(as_id(gdoc_id), type = "pdf", overwrite = overwrite_local)
                        if (log == TRUE) {
                                source('~/R/log_functions.R')
                                tell_me(get_gname_from_id(gdoc_id))
                                desc <- readline("Describe the file that was just downloaded from Google Drive. ")
                                write_log_entry(fn = fn,
                                                input_or_output = "from Google Drive",
                                                description = desc)
                        }
                }
                
        }

dl_gdoc_as_txt <-
        function(full_gdoc_dribble, remove_gdrive = FALSE, path_folder = "./", overwrite_local = TRUE, log = TRUE) {
                require(googledrive)
                gdoc_txt_fns <- paste0(path_folder, dedupe_vector(full_gdoc_dribble$name), ".txt")
                
                for (i in 1:length(gdoc_txt_fns)) {
                        gdoc_dribble <- full_gdoc_dribble[i,]
                        if (remove_gdrive == TRUE) {
                                if (log == TRUE) {
                                        source('~/R/log_functions.R')
                                        desc <- readline("Describe the file that was just downloaded from Google Drive. ")
                                        write_log_entry(fn = fn,
                                                        input_or_output = "from Google Drive",
                                                        description = desc)
                                }
                                drive_download(gdoc_dribble, type = "txt", path = gdoc_txt_fns[i], overwrite = overwrite_local)
                                drive_trash(gdoc_dribble) 
                        } else {
                                if (log == TRUE) {
                                        source('~/R/log_functions.R')
                                        desc <- readline("Describe the file that was just downloaded from Google Drive. ")
                                        write_log_entry(fn = fn,
                                                        input_or_output = "from Google Drive",
                                                        description = desc)
                                }
                                drive_download(gdoc_dribble, type = "txt", path = gdoc_txt_fns[i], overwrite = overwrite_local)
                        }
                }
                return(gdoc_txt_fns)
                
        }


