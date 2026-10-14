points <- function() {
    tibble::tibble(
        id = 1:3,
        latitude = c(29.65, 29.70, 45.00),
        longitude = c(-82.32, -82.30, -100.00)
    )
}

test_that("add_grid_cells() adds a cell and its centre to every row", {
    out <- add_grid_cells(points(), cellsize_km = 100)

    expect_named(out, c("id", "latitude", "longitude", "cell", "cell_lon", "cell_lat"))
    expect_equal(out$id, 1:3)
    # Nearby points share a cell; a distant point does not
    expect_equal(out$cell[1], out$cell[2])
    expect_false(out$cell[3] == out$cell[1])
    expect_equal(out$cell_lon[1], out$cell_lon[2])
})

test_that("add_grid_cells() puts each point within half a cell of its centre", {
    out <- add_grid_cells(points(), cellsize_km = 100)
    to_metres <- function(lon, lat) {
        terra::project(cbind(lon, lat), from = "EPSG:4326", to = "EPSG:8857")
    }
    offset <- abs(
        to_metres(out$longitude, out$latitude) - to_metres(out$cell_lon, out$cell_lat)
    )

    expect_true(all(offset <= 50000))
})

test_that("add_grid_cells() takes the latitude and longitude columns as arguments", {
    renamed <- dplyr::rename(points(), y = latitude, x = longitude)
    out <- add_grid_cells(renamed, lat = y, lon = x, cellsize_km = 100)

    expect_equal(out$cell_lat, add_grid_cells(points(), cellsize_km = 100)$cell_lat)
})

test_that("add_grid_cells() gives the same centres whatever the extent of the data", {
    all_points <- add_grid_cells(points(), cellsize_km = 100)
    two_points <- add_grid_cells(points()[1:2, ], cellsize_km = 100)

    expect_equal(two_points$cell_lon, all_points$cell_lon[1:2])
    expect_equal(two_points$cell_lat, all_points$cell_lat[1:2])
})

test_that("add_grid_cells() keeps rows with a missing coordinate as NA", {
    data <- points()
    data$latitude[2] <- NA
    out <- expect_no_warning(add_grid_cells(data, cellsize_km = 100))

    expect_equal(nrow(out), 3)
    expect_equal(is.na(out$cell), c(FALSE, TRUE, FALSE))
    expect_equal(is.na(out$cell_lat), c(FALSE, TRUE, FALSE))
})
