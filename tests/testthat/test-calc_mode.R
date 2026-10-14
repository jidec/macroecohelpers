test_that("calc_mode() returns the most common value", {
    expect_equal(calc_mode(c("a", "b", "b", "c")), "b")
    expect_equal(calc_mode(c(3, 1, 3, 2)), 3)
})

test_that("calc_mode() gives ties to the value seen first", {
    expect_equal(calc_mode(c(2, 1, 1, 2)), 2)
})

test_that("calc_mode() ignores missing values by default", {
    expect_equal(calc_mode(c(NA, NA, NA, "a")), "a")
})

test_that("calc_mode() counts missing values when na_rm is FALSE", {
    expect_equal(calc_mode(c(NA, NA, "a"), na_rm = FALSE), NA_character_)
})

test_that("calc_mode() returns a typed NA when there is nothing to count", {
    expect_identical(calc_mode(c(NA_character_, NA_character_)), NA_character_)
    expect_identical(calc_mode(numeric()), NA_real_)
    expect_identical(
        calc_mode(factor(NA, levels = c("a", "b"))),
        factor(NA, levels = c("a", "b"))
    )
})

test_that("calc_mode() keeps the type of its input", {
    x <- factor(c("b", "a", "b"), levels = c("a", "b", "c"))
    expect_identical(calc_mode(x), factor("b", levels = c("a", "b", "c")))

    dates <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-02"))
    expect_identical(calc_mode(dates), as.Date("2020-01-02"))
})

test_that("calc_mode() validates its arguments", {
    expect_error(calc_mode(1:3, na_rm = NA), "na_rm")
})
