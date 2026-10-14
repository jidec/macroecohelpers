# 15 April, when solar noon and mean noon nearly coincide
observations <- function() {
    tibble::tibble(
        id = 1:3,
        datetime = as.POSIXct(
            c("2021-04-15 12:00:00", "2021-04-15 18:00:00", "2021-04-15 09:00:00"),
            tz = "UTC"
        ),
        longitude = c(0, -90, 45)
    )
}

test_that("adds solar_time and keeps every row in order", {
    out <- add_solartime(observations())

    expect_s3_class(out, "tbl_df")
    expect_named(out, c("id", "datetime", "longitude", "solar_time"))
    expect_equal(out$id, 1:3)
})

test_that("solar time shifts by one hour per 15 degrees of longitude", {
    out <- add_solartime(observations())

    # Noon at Greenwich, noon 90 degrees west six hours later, noon 45 degrees east
    expect_equal(out$solar_time, c(12, 12, 12), tolerance = 0.01)
})

test_that("solar time follows the equation of time through the year", {
    data <- tibble::tibble(
        datetime = as.POSIXct(c("2021-02-11 12:00:00", "2021-11-03 12:00:00"), tz = "UTC"),
        longitude = 0
    )
    out <- add_solartime(data)

    # The sun runs about 14 minutes slow in mid February and 16 fast in early November
    expect_equal(out$solar_time, c(12 - 14 / 60, 12 + 16 / 60), tolerance = 0.005)
})

test_that("solar time is wrapped into 0 to 24", {
    data <- tibble::tibble(
        datetime = as.POSIXct(c("2021-04-15 02:00:00", "2021-04-15 22:00:00"), tz = "UTC"),
        longitude = c(-90, 90)
    )
    out <- add_solartime(data)

    expect_equal(out$solar_time, c(20, 4), tolerance = 0.01)
})

test_that("the time zone the datetime is stored in does not change the result", {
    data <- observations()
    expected <- add_solartime(observations())$solar_time

    # Asia/Kolkata is offset from UTC by a half hour
    for (tz in c("America/New_York", "Asia/Kolkata")) {
        data$datetime <- lubridate::with_tz(data$datetime, tz)
        expect_equal(add_solartime(data)$solar_time, expected)
    }
})

test_that("takes the datetime and longitude columns as arguments", {
    renamed <- dplyr::rename(observations(), observed_at = datetime, x = longitude)
    out <- add_solartime(renamed, datetime = observed_at, lon = x)

    expect_equal(out$solar_time, add_solartime(observations())$solar_time)
})

test_that("rows with a missing time or longitude get NA", {
    data <- observations()
    data$datetime[1] <- NA
    data$longitude[2] <- NA
    out <- add_solartime(data)

    expect_equal(is.na(out$solar_time), c(TRUE, TRUE, FALSE))
})
