is_google_id <-
        function(char_string) {
                ifelse(((get_nchar_decimal(char_string)) == 0) &
                               ((get_nchar_space(char_string)) == 0) &
                               ((grepl("[a-z]{1,}[A-Z]{1,}", char_string)) == TRUE) &
                               ((get_nchar_fslash(char_string)) == 0),
                       return(TRUE),
                       return(FALSE))
        }


is_google_url <-
        function(char_string) {
                ifelse(((get_nchar_decimal(char_string)) > 0) &
                               ((grepl("drive\\.google\\.com", char_string)) == TRUE) &
                               ((get_nchar_punct(char_string)) >= 1),
                       return(TRUE),
                       return(FALSE))
        }


get_names_from_dribble <-
        function(dribble) {
                require(googledrive)
                dribble %>%
                        select(name) %>%
                        unlist() %>%
                        unname()
        }

get_ids_from_dribble <-
        function(dribble) {
                require(googledrive)
                dribble %>%
                        select(id) %>%
                        unlist() %>%
                        unname()
        }

get_file_info_from_dribble <-
        function(dribble, full = FALSE) {
                drive_resource_list <- dribble$drive_resource
                drive_resource_list <- lapply(1:length(drive_resource_list), function(i) bind_rows(unlist(drive_resource_list[[i]])))
                drive_resource_dataframe <- bind_rows(drive_resource_list)
                
                if (full) {
                        dribble %>%
                                merge(drive_resource_dataframe) %>%
                                call_mr_clean()
                } else {
                        dribble %>%
                                merge(drive_resource_dataframe) %>%
                                select(name, id, kind, mimeType) 
                }
        }

get_gdrive_dribble <-
        function(gdrive_identifier = "root") {
                if (gdrive_identifier == "root") {
                        drive_get(gdrive_identifier)
                } else if (is_google_id(gdrive_identifier)) {
                        drive_get(as_id(gdrive_identifier))
                } else if (is_google_url(gdrive_identifier)) {
                        tell_me("Please reenter with id and not url.")
                }
        }

get_gfolder_dribble <-
        function(gfolder_identifier = "root", verbose = TRUE) {
                if (gfolder_identifier == "root") {
                        drive_ls("root", verbose = verbose)
                } else if (is_google_id(gfolder_identifier)) {
                        drive_ls(as_id(gfolder_identifier), verbose = verbose)
                } else if (is_google_url(gfolder_identifier)) {
                        tell_me("Please reenter with id and not url.")
                }
        }
