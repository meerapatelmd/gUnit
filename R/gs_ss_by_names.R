#' Gets a list of googlesheet metadata from a list of gsheet names (exact string match)
#' @param gsheet_names character vector of gsheet names that are an exact string match
#' @import googlesheets
#' @export

gs_ss_by_names <-
        function(gsheet_names) {
                x <- list()
                for (i in 1:length(gsheet_names)) {
                        gsheet_id <- gs_id_from_name(gsheet_names[i])
                        if (!(is.na(gsheet_id))) {
                                x[[i]] <- googlesheets::gs_key(gsheet_id)
                                names(x)[i] <- gsheet_names[i]
                        } else {
                                x[[i]] <- NA
                                names(x)[i] <- gsheet_names[i]
                        }
                }
                return(x)
        }
