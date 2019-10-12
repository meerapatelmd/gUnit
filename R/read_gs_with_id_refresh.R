#' Reads a googlesheet whilst also replacing NA or blank "TIMESTAMP" values as current timestamp
#' @param gsheet_id google sheet id
#' @import gXtra
#' @import dplyr
#' @import googlesheets
#' @export

read_gs_with_id_refresh <-
        function(gsheet_id) {
                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                id_column_name <- grep("_ID", colnames(data), value = TRUE)

                id_column_name <- enquo(id_column_name)

                old_id_column_values <-
                        data %>%
                        dplyr::select(!!id_column_name) %>%
                        unlist()

                new_id_column_values <- caterpillaR::carry_forward_and_add_one(old_id_column_values)

                for (i in 1:nrow(data)) {
                        ID <- old_id_column_values[i]
                        if (is.na(ID)|ID == ""|ID == "NA") {
                                index <- i + 1
                                col_letter <- LETTERS[grep("ID", colnames(data))]
                                anchor <- paste0(col_letter, index)

                                googlesheets::gs_edit_cells(gXtra::get_gsheet_metadata(gsheet_id),
                                                            anchor = anchor,
                                                            input = new_id_column_values[i])

                                Sys.sleep(3)
                                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                                Sys.sleep(3)
                        }
                }
                data <- gXtra::read_gs(gXtra::get_gsheet_metadata(gsheet_id))
                Sys.sleep(3)
                return(data)
        }




