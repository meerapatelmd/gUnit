#' Reads a googlesheet whilst also replacing NA or blank "TIMESTAMP" values as current timestamp
#' @param gsheet_id google sheet id
#' @import gXtra
#' @import dplyr
#' @import googlesheets
#' @export

read_gs_with_timestamp_and_id_refresh <-
        function(gsheet_id) {
                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                for (i in 1:nrow(data)) {
                        TIMESTAMP <- data %>%
                                dplyr::select(dplyr::contains("TIMESTAMP")) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()
                        if (is.na(TIMESTAMP)|TIMESTAMP == "") {
                                index <- i + 1
                                col_letter <- LETTERS[grep("TIMESTAMP", colnames(data))]
                                anchor <- paste0(col_letter, index)
                                googlesheets::gs_edit_cells(gXtra::get_gsheet_metadata(gsheet_id),
                                                            anchor = anchor,
                                                            input = mirroR::get_timestamp())
                                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                        }
                }

                for (i in 1:nrow(data)) {
                        ID <- data %>%
                                dplyr::select(dplyr::contains("_ID")) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()
                        if (is.na(ID)|ID == "") {
                                index <- i
                                col_letter <- LETTERS[grep("_ID", colnames(data))]
                                anchor <- paste0(col_letter, index)

                                input <- 1 + as.integer(data %>%
                                                                dplyr::select(dplyr::contains("_ID")) %>%
                                                                dplyr::filter(row_number() == (i-1)) %>%
                                                                unlist())

                                googlesheets::gs_edit_cells(gXtra::get_gsheet_metadata(gsheet_id),
                                                            anchor = anchor,
                                                            input = input)
                                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                        }
                }
                return(data)
        }


