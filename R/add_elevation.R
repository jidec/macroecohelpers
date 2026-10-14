#' Add the elevation of each point
#'
#' Looks up the elevation at each point with [elevatr::get_elev_point()] and
#' adds it as an `elevation` column, in metres. This downloads elevation
#' tiles, so it needs an internet connection.
#'
#' @param data A data frame.
#' @param lat,lon <[`data-masking`][dplyr::dplyr_data_masking]> Columns
#'   holding latitude and longitude in decimal degrees (WGS84).
#' @param ... Other arguments passed to [elevatr::get_elev_point()], such as
#'   `z`, the zoom level that sets the resolution of the tiles.
#'
#' @returns A tibble with the same rows as `data` and an added `elevation`
#'   column. Rows with a missing coordinate get `NA`.
#'
#' @examples
#' \dontrun{
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "B. nigra"),
#'     decimalLatitude = c(29.65, 39.74),
#'     decimalLongitude = c(-82.32, -104.99)
#' )
#'
#' add_elevation(occurrences, lat = decimalLatitude, lon = decimalLongitude)
#'
#' add_elevation(occurrences, lat = decimalLatitude, lon = decimalLongitude, z = 10)
#' }
#' @export
add_elevation <- function(data, lat = latitude, lon = longitude, ...) {
    # Points with both coordinates
    points <- data.frame(x = dplyr::pull(data, {{ lon }}), y = dplyr::pull(data, {{ lat }}))
    ok <- stats::complete.cases(points)

    # Rows with a missing coordinate stay NA
    elevation <- rep(NA_real_, nrow(points))
    elevation[ok] <- fetch_elevation(points[ok, ], ...)

    data |>
        dplyr::mutate(elevation = elevation) |>
        tibble::as_tibble()
}

# Elevation in metres at each row of `points` (columns x and y, WGS84)
fetch_elevation <- function(points, ...) {
    rlang::check_installed(c("elevatr", "sf"))
    elevatr::get_elev_point(points, prj = "EPSG:4326", src = "aws", ...)$elevation
}
