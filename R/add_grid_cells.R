#' Add the equal-area grid cell that each point falls in
#'
#' Lays a grid of square cells over the points and adds each point's cell ID
#' and the coordinates of the cell's centre. The grid is built in an
#' equal-area projection, so cells are `cellsize_km` wide everywhere, not a
#' fixed number of degrees.
#'
#' Cell centres do not depend on the extent of the data, but cell IDs do, so
#' compare IDs only within one call.
#'
#' @param df A data frame.
#' @param lat,lon <[`data-masking`][dplyr::dplyr_data_masking]> Columns
#'   holding latitude and longitude in decimal degrees (WGS84).
#' @param cellsize_km Width of a grid cell in kilometres.
#' @param crs Projected CRS, in metres, that the grid is built in. The default
#'   is Equal Earth.
#'
#' @returns `df` with three added columns: `cell`, the cell ID, and `cell_lon`
#'   and `cell_lat`, the cell centre in decimal degrees. Rows with a missing
#'   coordinate get `NA`.
#'
#' @examplesIf rlang::is_installed("terra")
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "A. alba", "B. nigra"),
#'     decimalLatitude = c(29.65, 29.70, 45.00),
#'     decimalLongitude = c(-82.32, -82.30, -100.00)
#' )
#'
#' add_grid_cells(
#'     occurrences,
#'     lat = decimalLatitude,
#'     lon = decimalLongitude,
#'     cellsize_km = 100
#' )
#' @export
add_grid_cells <- function(df, lat = latitude, lon = longitude, cellsize_km = 250, crs = "EPSG:8857") {
    rlang::check_installed("terra")

    # Points with both coordinates, projected to the grid's CRS in metres
    lonlat <- cbind(dplyr::pull(df, {{ lon }}), dplyr::pull(df, {{ lat }}))
    ok <- stats::complete.cases(lonlat)
    xy <- terra::project(lonlat[ok, , drop = FALSE], from = "EPSG:4326", to = crs)
    size <- cellsize_km * 1000

    # Grid covering the points, with edges on multiples of the cell size
    grid <- terra::rast(
        xmin = (floor(min(xy[, 1]) / size) - 1) * size,
        xmax = (ceiling(max(xy[, 1]) / size) + 1) * size,
        ymin = (floor(min(xy[, 2]) / size) - 1) * size,
        ymax = (ceiling(max(xy[, 2]) / size) + 1) * size,
        resolution = size,
        crs = crs
    )

    # Cell each point falls in, and its centre back in degrees
    cell <- terra::cellFromXY(grid, xy)
    centers <- terra::project(terra::xyFromCell(grid, cell), from = crs, to = "EPSG:4326")

    # Rows with a missing coordinate stay NA
    df$cell <- replace(rep(NA_real_, nrow(df)), ok, cell)
    df$cell_lon <- replace(rep(NA_real_, nrow(df)), ok, centers[, 1])
    df$cell_lat <- replace(rep(NA_real_, nrow(df)), ok, centers[, 2])
    df
}
