is_gs_key <-
        function(char_string) {
                ifelse(((get_nchar_decimal(char_string)) == 0) &
                               ((get_nchar_space(char_string)) == 0) &
                               ((grepl("[a-z]{1,}[A-Z]{1,}", char_string)) == TRUE) &
                               ((get_nchar_fslash(char_string)) == 0),
                       return(TRUE),
                       return(FALSE))
        }

get_gsheet_ss <-
        function(gsheet_identifier) {
                if (grepl("docs\\.google\\.com\\/spreadsheets", gsheet_identifier)) {
                        gs_url(gsheet_identifier)
                } else if (is_gs_key(gsheet_identifier)) {
                        gs_key(gsheet_identifier)
                } else {
                        gs_key(get_gsheet_key_from_gsheet_name(gsheet_identifier))
                }
        }

get_gsheet_name_from_identifier <-
        function(gsheet_identifier) {
                ss <- get_gsheet_ss(gsheet_identifier)
                return(ss$sheet_title)
        }

get_gsheet_url_from_identifier <-
        function(gsheet_identifier) {
                ss <- get_gsheet_ss(gsheet_identifier)
                return(ss$browser_url)
        }

get_gsheet_key_from_url <-
        function(gsheet_url) {
                ss <- get_gsheet_ss(gsheet_url)
                return(ss$sheet_key)
        }

get_gsheet_key_from_gsheet_name <-
        function(gsheet_name) {
                ss <- gs_key((get_gsheet_list()[get_gsheet()$name == gsheet_name,"id"] %>%
                                      unname() %>%
                                      unlist())[1])
                ss$sheet_key
        }

get_gsheet_name_from_gsheet_ss <-
        function(gsheet_ss) {
                return(gsheet_ss$sheet_title)
        }


get_gsheet_key_from_gsheet_ss <- 
        function(gsheet_ss) {
                return(gsheet_ss$sheet_key)
        }

get_gsheet_url_from_gsheet_ss <-
        function(gsheet_ss) {
                return(gsheet_ss$browser_url)
        }