occurrences <- function() {
    tibble::tibble(
        taxon = c("b", "b", "a", "a", "a"),
        family = c("F1", "F1", "F1", NA, "F1"),
        sex = c("female", NA, "male", "male", "female"),
        latitude = c(10, 20, 1, NA, 3),
        captive = c(TRUE, FALSE, FALSE, FALSE, NA)
    )
}

test_that("returns a tibble with one row per group and the group size", {
    out <- summarize_by(occurrences(), by = taxon)

    expect_s3_class(out, "tbl_df")
    expect_named(
        out,
        c("taxon", "sex_female", "sex_male", "family", "latitude", "captive", "n")
    )
    expect_equal(out$taxon, c("b", "a"))
    expect_equal(out$n, c(2L, 3L))
})

test_that("numeric columns become the mean and logical columns the proportion TRUE", {
    out <- summarize_by(occurrences(), by = taxon)

    expect_equal(out$latitude, c(15, 2))
    expect_equal(out$captive, c(0.5, 0))
})

test_that("a character column with few values becomes a proportion per value", {
    out <- summarize_by(occurrences(), by = taxon)

    # Group b has one female and one NA; the NA is not in the denominator
    expect_equal(out$sex_female, c(1, 1 / 3))
    expect_equal(out$sex_male, c(0, 2 / 3))
    expect_false("sex" %in% names(out))
})

test_that("a factor column with few values becomes a proportion per value", {
    data <- tibble::tibble(
        g = c("a", "a", "b"),
        stage = factor(c("adult", "larva", "adult"))
    )
    out <- summarize_by(data, by = g)

    expect_equal(out$stage_adult, c(0.5, 1))
    expect_equal(out$stage_larva, c(0.5, 0))
})

test_that("blank strings are treated as missing in proportion columns", {
    data <- tibble::tibble(
        g = c("a", "a", "a", "b", "b"),
        sex = c("male", "female", "", "male", ""),
        stage = factor(c("adult", "", "larva", "adult", "adult"))
    )
    out <- summarize_by(data, by = g, max_levels = 2)

    expect_named(out, c("g", "sex_female", "sex_male", "stage_adult", "stage_larva", "n"))
    expect_equal(out$sex_female, c(0.5, 0))
    expect_equal(out$sex_male, c(0.5, 1))
    expect_equal(out$stage_adult, c(0.5, 1))
    expect_equal(out$stage_larva, c(0.5, 0))
})

test_that("single-valued and many-valued columns become their most common value", {
    data <- tibble::tibble(
        g = c("a", "a", "a", "b"),
        family = "F1",
        id = c("x", "y", "y", "z"),
        date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-02", "2021-06-01"))
    )
    out <- summarize_by(data, by = g, max_levels = 2)

    expect_identical(out$family, c("F1", "F1"))
    expect_identical(out$id, c("y", "z"))
    expect_identical(out$date, as.Date(c("2020-01-02", "2021-06-01")))
})

test_that("max_levels sets how many values a column can have and still be expanded", {
    data <- tibble::tibble(g = "a", id = c("x", "y", "z"))

    expect_named(summarize_by(data, by = g, max_levels = 2), c("g", "id", "n"))
    expect_named(
        summarize_by(data, by = g, max_levels = 3),
        c("g", "id_x", "id_y", "id_z", "n")
    )
})

test_that("by accepts several columns, which are not expanded", {
    out <- summarize_by(occurrences(), by = c(taxon, sex))

    expect_equal(out$taxon, c("b", "b", "a", "a"))
    expect_equal(out$sex, c("female", NA, "male", "female"))
    expect_equal(out$latitude, c(10, 20, 1, 3))
    expect_equal(out$n, c(1L, 1L, 2L, 1L))
    expect_false(any(c("sex_female", "sex_male") %in% names(out)))
})

test_that("an expression in ... replaces the default for that column", {
    out <- summarize_by(
        occurrences(),
        by = taxon,
        latitude = max(latitude, na.rm = TRUE),
        sex = calc_mode(sex)
    )

    expect_equal(out$latitude, c(20, 3))
    expect_equal(out$sex, c("female", "male"))
    expect_false(any(c("sex_female", "sex_male") %in% names(out)))
})

test_that("an expression in ... can add a column computed from the raw values", {
    out <- summarize_by(
        occurrences(),
        by = taxon,
        latitude_range = max(latitude, na.rm = TRUE) - min(latitude, na.rm = TRUE)
    )

    expect_equal(out$latitude_range, c(10, 2))
    expect_equal(out$latitude, c(15, 2))
})

test_that("a base data frame is accepted and a tibble is returned", {
    data <- data.frame(g = c("a", "a"), x = c(1, 2))

    expect_s3_class(summarize_by(data, by = g), "tbl_df")
})
