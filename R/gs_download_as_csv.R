#' Downloads all the tabs in a given Google Sheet as csvs to destination path with filename as the tab name
#' @importFrom googlesheets gs_download
#' @export

gs_download_as_csv <-
        function(gsheet_name, dest_path, prefix_csv_fn, system.sleep = 2) {
                tab_names <- mirCat::gs_tab_names(gsheet_name)
                for (i in 1:length(tab_names)) {
                        tab_name <- tab_names[i]
                        full_path <- paste0(dest_path, "/", prefix_csv_fn, tab_name, ".csv")
                        googlesheets::gs_download(mirCat::gs_ss_by_name(gsheet_name), ws = tab_name, to = full_path)
                        Sys.sleep(system.sleep)
                }

        }
