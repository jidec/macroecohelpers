occurrences <- function() {
    tibble::tibble(
        taxon = c("a", "a", "b", "b"),
        latitude = c(10, 20, 30, 40),
        elevation = c(1, 2, 3, NA),
        captive = c(TRUE, FALSE, FALSE, TRUE)
    )
}

test_that("every numeric column gets a _sc copy and the originals are unchanged", {
    out <- add_scaled(occurrences())

    expect_s3_class(out, "tbl_df")
    expect_named(
        out,
        c("taxon", "latitude", "elevation", "captive", "latitude_sc", "elevation_sc")
    )
    expect_identical(out[names(occurrences())], occurrences())
})

test_that("scaled columns have mean 0 and standard deviation 1", {
    out <- add_scaled(occurrences())

    expect_equal(out$latitude_sc, as.numeric(scale(c(10, 20, 30, 40))))
    expect_equal(mean(out$latitude_sc), 0)
    expect_equal(sd(out$latitude_sc), 1)
})

test_that("missing values are ignored and stay missing", {
    out <- add_scaled(occurrences())

    expect_equal(out$elevation_sc, c(-1, 0, 1, NA))
})

test_that("cols chooses which columns are scaled", {
    out <- add_scaled(occurrences(), cols = latitude)

    expect_named(out, c(names(occurrences()), "latitude_sc"))

    out <- add_scaled(occurrences(), cols = c(latitude, captive))

    expect_equal(out$captive_sc, as.numeric(scale(c(1, 0, 0, 1))))
})

test_that("a base data frame is accepted and a tibble is returned", {
    data <- data.frame(g = c("a", "b"), x = c(1, 2))

    expect_s3_class(add_scaled(data), "tbl_df")
})
