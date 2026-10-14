points <- function() {
    tibble::tibble(
        id = 1:3,
        latitude = c(29.65, 39.74, 45.00),
        longitude = c(-82.32, -104.99, -100.00)
    )
}

# Swaps the download for `fake` until the calling test ends. The installed
# testthat (3.1.6) predates local_mocked_bindings(), which would replace this.
local_fake_elevation <- function(fake = function(points, ...) points$y * 10, env = parent.frame()) {
    ns <- asNamespace("macroecohelpers")
    real <- ns$fetch_elevation
    set <- function(f) {
        unlockBinding("fetch_elevation", ns)
        assign("fetch_elevation", f, envir = ns)
    }
    set(fake)
    withr::defer(set(real), envir = env)
}

test_that("adds an elevation column and keeps every row in order", {
    local_fake_elevation()
    out <- add_elevation(points())

    expect_s3_class(out, "tbl_df")
    expect_named(out, c("id", "latitude", "longitude", "elevation"))
    expect_equal(out$id, 1:3)
    expect_equal(out$elevation, c(296.5, 397.4, 450))
})

test_that("takes the latitude and longitude columns as arguments", {
    local_fake_elevation()
    renamed <- dplyr::rename(points(), y_deg = latitude, x_deg = longitude)
    out <- add_elevation(renamed, lat = y_deg, lon = x_deg)

    expect_equal(out$elevation, c(296.5, 397.4, 450))
})

test_that("rows with a missing coordinate get NA and are not looked up", {
    looked_up <- NULL
    local_fake_elevation(function(points, ...) {
        looked_up <<- points
        points$y * 10
    })
    data <- points()
    data$longitude[2] <- NA
    out <- add_elevation(data)

    expect_equal(nrow(looked_up), 2)
    expect_equal(out$elevation, c(296.5, NA, 450))
})

test_that("extra arguments are passed on to the lookup", {
    local_fake_elevation(function(points, z) rep(z, nrow(points)))

    expect_equal(add_elevation(points(), z = 10)$elevation, c(10, 10, 10))
})

test_that("the download is restored after a test that faked it", {
    expect_match(deparse(body(fetch_elevation)), "get_elev_point", all = FALSE)
})

test_that("looks up real elevations", {
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("elevatr")
    out <- add_elevation(points()[1:2, ])

    # Gainesville, Florida is near sea level; Denver, Colorado is about 1600 m
    expect_lt(out$elevation[1], 100)
    expect_gt(out$elevation[2], 1400)
})
