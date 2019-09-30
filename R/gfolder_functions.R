source('~/R/dribble_functions.R')

exists_in_gfolder <-
        function(gfolder_id, filename) {
                require(googledrive)
                dribble <-
                        get_gfolder_dribble(gfolder_id = gfolder_id, verbose = FALSE)
                if (filename %in% get_gname_from_dribble(dribble)) {
                        return(TRUE)
                } else {
                        return(FALSE)
                }
        }

get_gfolder_file_count <-
        function(gfolder_id, n_max = Inf, ...) {
                require(googledrive)
                nrow(drive_ls(path = as_id(gfolder_id), n_max = n_max))
        }


mk_gfolder <-
        function(new_gfolder_name, parent_gfolder_id = NULL) {
                drive_mkdir(name = new_gfolder_name, parent = as_id(parent_gfolder_id))
        }


cp_gfolder <-
        function(origin_gfolder_id,
                 dest_gfolder_id,
                 new_name = "exact", 
                 prefix = NULL, 
                 suffix = NULL) {
                origin_dribble <- drive_ls(as_id(origin_gfolder_id))
                if (new_name == "exact") {
                        for (i in 1:nrow(origin_dribble)) {
                                drive_cp(file = as_dribble(origin_dribble[i,]),
                                         path = as_id(dest_gfolder_id),
                                         name = origin_dribble$name[i])
                        }
                } else if (new_name == "default") {
                        for (i in 1:nrow(origin_dribble)) {
                                drive_cp(file = as_dribble(origin_dribble[i,]),
                                         path = as_id(dest_gfolder_id),
                                         name = origin_dribble$name[i])
                        }
                } else if (!(is.null(prefix))) {
                        for (i in 1:nrow(origin_dribble)) {
                                drive_cp(file = as_dribble(origin_dribble[i,]),
                                         path = as_id(dest_gfolder_id),
                                         name = paste(prefix, origin_dribble$name[i]), sep = " ", collapse = " ")
                        }
                } else if (!(is.null(suffix))) {
                        for (i in 1:nrow(origin_dribble)) {
                                drive_cp(file = as_dribble(origin_dribble[i,]),
                                         path = as_id(dest_gfolder_id),
                                         name = paste(origin_dribble$name[i]), suffix, sep = " ", collapse = " ")
                        }
                }
        }

## Make `Deprecated` folder
make_deprecated_sub_gdir <-
        function(gfolder_id) {
                if (exists_in_gfolder(gfolder_id = gfolder_id, "Deprecated")) {
                        tell_me("Deprecated folder exists.")
                        make_new_line()
                        stop_and_enter()
                } else {
                        mk_gfolder("Deprecated", parent_gfolder_id = gfolder_id)
                }
        }


# Combining files in 2 folders into 1 new folder
combine_gfolders <-
        function(new_gfolder_name,
                 gfolder_id_1,
                 gfolder_id_2,
                 new_name = "exact", 
                 prefix = NULL, 
                 suffix = NULL,
                 deprecate = TRUE) {
                require(googledrive)
                
                new_gfolder_dribble <- mk_gfolder(new_gfolder_name = new_gfolder_name)
                new_gfolder_id <- get_google_ids_from_dribble(new_gfolder_dribble)
                
                origin_gfolder_ids <- c(gfolder_id_1, gfolder_id_2)
                lapply(origin_gfolder_ids, cp_gfolder, dest_gfolder_id = new_gfolder_id)
                
                if (deprecate == TRUE) {
                        deprecated_folder_id <- mk_gfolder("Deprecated", parent = as_id(new_gfolder_id))
                        
                        for (i in 1:length(origin_gfolder_ids)) {
                                gfolder_name <- get_google_name_from_gid(origin_gfolder_ids[i])
                                gfolder_id <- mk_gfolder(gfolder_name, parent = as_id(deprecated_folder_id))
                                gfolder_cp(origin_gfolder_id = origin_gfolder_ids[i],
                                           dest_gfolder_id = gfolder_id)
                                drive_rm(as_id(origin_gfolder_ids[i]))
                        }
                }
                
        }
