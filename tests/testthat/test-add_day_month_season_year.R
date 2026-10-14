observations <- function() {
    tibble::tibble(
        id = 1:3,
        datetime = as.POSIXct(
            c("2021-01-15 09:00:00", "2021-06-01 14:30:00", "2020-12-31 17:00:00"),
            tz = "UTC"
        )
    )
}

test_that("adds day, month, season, and year and keeps every row in order", {
    out <- add_day_month_season_year(observations())

    expect_s3_class(out, "tbl_df")
    expect_named(out, c("id", "datetime", "day", "month", "season", "year"))
    expect_equal(out$id, 1:3)
    expect_identical(out$month, c(1L, 6L, 12L))
    expect_identical(out$year, c(2021L, 2021L, 2020L))
})

test_that("day is the day of the year", {
    out <- add_day_month_season_year(observations())

    # 2020 is a leap year, so 31 December is day 366
    expect_identical(out$day, c(15L, 152L, 366L))
})

test_that("every month falls in its meteorological season", {
    data <- tibble::tibble(datetime = as.Date(sprintf("2021-%02d-15", 1:12)))
    out <- add_day_month_season_year(data)

    expect_equal(
        out$season,
        c(
            "winter", "winter", "spring", "spring", "spring", "summer",
            "summer", "summer", "fall", "fall", "fall", "winter"
        )
    )
})

test_that("seasons change on the first of the month, in leap years too", {
    data <- tibble::tibble(
        datetime = as.Date(c("2020-02-29", "2020-03-01", "2021-02-28", "2021-03-01"))
    )

    expect_equal(
        add_day_month_season_year(data)$season,
        c("winter", "spring", "winter", "spring")
    )
})

test_that("values are read in the time zone the datetime is stored in", {
    data <- tibble::tibble(
        datetime = as.POSIXct("2021-01-01 02:00:00", tz = "UTC")
    )
    data$datetime <- lubridate::with_tz(data$datetime, "America/New_York")
    out <- add_day_month_season_year(data)

    # Still 31 December 2020 in New York
    expect_identical(out$day, 366L)
    expect_identical(out$month, 12L)
    expect_identical(out$year, 2020L)
})

test_that("takes the datetime column as an argument", {
    renamed <- dplyr::rename(observations(), observed_at = datetime)
    out <- add_day_month_season_year(renamed, datetime = observed_at)

    expect_equal(out$season, c("winter", "summer", "winter"))
})

test_that("rows with a missing datetime get NA", {
    data <- observations()
    data$datetime[2] <- NA
    out <- add_day_month_season_year(data)

    expect_equal(nrow(out), 3)
    expect_identical(out$day, c(15L, NA, 366L))
    expect_equal(out$season, c("winter", NA, "winter"))
})
