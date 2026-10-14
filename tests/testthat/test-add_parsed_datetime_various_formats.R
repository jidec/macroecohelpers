utc <- function(x) as.POSIXct(x, tz = "UTC")

test_that("add_parsed_datetime_various_formats() parses mixed formats", {
    data <- tibble::tibble(
        date = c(
            "2021-06-01 14:30:15",
            "2021/06/01 14:30:15",
            "2021/06/01 2:30:15 PM",
            "Tue Jun 01 2021 14:30:15",
            "2021-06-01T14:30:15Z"
        )
    )
    out <- add_parsed_datetime_various_formats(data)

    expect_equal(out$datetime, rep(utc("2021-06-01 14:30:15"), 5))
})

test_that("add_parsed_datetime_various_formats() parses dates without seconds or a time", {
    data <- tibble::tibble(
        date = c("2021/06/01 2:30 PM", "2021/06/01 14:30", "2021-06-01", "2021/06/01")
    )
    out <- add_parsed_datetime_various_formats(data)

    expect_equal(
        out$datetime,
        utc(c(
            "2021-06-01 14:30:00", "2021-06-01 14:30:00",
            "2021-06-01 00:00:00", "2021-06-01 00:00:00"
        ))
    )
})

test_that("add_parsed_datetime_various_formats() applies numeric UTC offsets", {
    data <- tibble::tibble(
        date = c(
            "2021-06-01 14:30:00 +0200",
            "2021-06-01T14:30:00-04:00",
            "Tue Jun 01 2021 14:30:00 GMT-0400 (EDT)"
        )
    )
    out <- add_parsed_datetime_various_formats(data)

    expect_equal(
        out$datetime,
        utc(c("2021-06-01 12:30:00", "2021-06-01 18:30:00", "2021-06-01 18:30:00"))
    )
})

test_that("add_parsed_datetime_various_formats() takes the date column as an argument", {
    data <- tibble::tibble(id = 1:2, observed_on = c("2021-06-01", "2021/06/02 9:00 AM"))
    out <- add_parsed_datetime_various_formats(data, date = observed_on)

    expect_named(out, c("id", "observed_on", "datetime"))
    expect_equal(out$datetime, utc(c("2021-06-01 00:00:00", "2021-06-02 09:00:00")))
})

test_that("add_parsed_datetime_various_formats() keeps unparseable dates as NA", {
    data <- tibble::tibble(date = c("2021-06-01", "not a date", NA))
    out <- add_parsed_datetime_various_formats(data)

    expect_equal(nrow(out), 3)
    expect_equal(is.na(out$datetime), c(FALSE, TRUE, TRUE))
})
