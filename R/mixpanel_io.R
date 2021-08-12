#' @rdname io
#'
#' @title Retrieve and read MixPanel CSV files
#'
#' @details `retrieve()` visits the Google bucket where AnVIL MixPanel
#'     data are stored, and retrieves all `*.csv` files to a local
#'     cache. Files already present in the cache are not
#'     re-localized. You will be prompted to authenticate with an
#'     account for which shared access to the `MixPanel 2021` Google
#'     Drive folder.
#'
#' @param destination character(1) location of the local cache. The
#'     default is the standard operating system location for R
#'     packages.
#'
#' @return `retrieve()` returns a tibble with columns year, month,
#'     title, and path to the localized file. The year, month, and
#'     title are parsed from the file name.
#'
#' @import futile.logger
#'
#' @importFrom tools R_user_dir
#'
#' @importFrom googledrive as_id drive_ls drive_download
#'
#' @importFrom dplyr .data filter mutate select arrange anti_join
#'     bind_rows everything
#'
#' @importFrom tibble tibble
#'
#' @export
retrieve <-
    function(destination = R_user_dir("mixpanel", "cache"))
{
    if (!dir.exists(destination))
        dir.create(destination)
    MixPanel_2021_id <- as_id("1JzgYx1QQYDPZRZjkbqzMHy5jH0JD9R7i")
    files <- drive_ls(MixPanel_2021_id)
    csv_files <- filter(files, endsWith(.data$name, ".csv"))

    current <- tibble(name = dir(destination, pattern = "*.csv"))
    download <- anti_join(csv_files, current, by = "name")
    flog.info(
        "discovered %d files, downloading %d",
        nrow(csv_files), nrow(download)
    )
    path <- file.path(destination, download$name)
    status <- Map(drive_download, download$id, path)

    tbl <- tibble(path = file.path(destination, csv_files$name))
    tbl |>
        mutate(
            month = factor(substr(basename(path), 1, 3), levels = month.abb),
            year = substr(basename(path), 4, 7),
            title = substr(basename(path), 9, nchar(basename(path)) - 4L)
        ) |>
        select(.data$year, .data$month, .data$title, .data$path) |>
        arrange(.data$year, .data$month, .data$title)
}

#' @rdname io
#'
#' @details `read()` reads several csv files into tibbles, and then
#'     binds rows into a single tibble. Column headers with a hyphen `
#'     - `, typically separating date ranges spanning the month, are
#'     replaced with `DateRangeCount`.
#'
#' @param tbl A tibble returned by `retrieve()`, typically filtered to
#'     select a title of interest.
#'
#' @return `read()` returns a single tibble.
#'
#' @importFrom readr read_csv
#'
#' @export
read <-
    function(tbl)
{
    flog.info("reading %d csv files", nrow(tbl))
    tbls <- lapply(tbl$path, function(path) {
        tbl <- read_csv(path, show_col_types = FALSE)
        ## re-name date ranges as 'DateRangeCount'
        idx <- grepl(" - ", names(tbl), fixed = TRUE)
        names(tbl)[idx] <- "DateRangeCount"
        tbl
    })
    nrows <- vapply(tbls, nrow, integer(1))
    bind_rows(tbls) |>
        mutate(Month = rep(tbl$month, nrows)) |>
        select(.data$Month, everything())
}        
