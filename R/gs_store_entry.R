
store_raw_product_info <-
        function(company_name,
                 product_name,
                 ingredients_string) {
                
                gsheet_id <- gUnit::get_gsheet_id_from_name("MASTER_COSMETIC_MINER")
                gsheet_metadata <- googlesheets::gs_key(gsheet_id)
                googlesheets::gs_add_row(gsheet_metadata,
                                         ws = "RAW_INPUT",
                                         input = c(company_name,
                                                   product_name,
                                                   ingredients_string))
        }
