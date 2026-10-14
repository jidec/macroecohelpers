render_dir <- function(files) {
    dir <- tempfile()
    dir.create(dir)
    file.create(file.path(dir, files))
    dir
}

test_that("add_critterframe_render_paths() matches renders to species written with spaces", {
    dir <- render_dir(c(
        "Acanthagrion_adustum__5759580977.png",
        "Acanthaeschna_victoria__4982372807.png"
    ))
    data <- tibble::tibble(
        species = c("Acanthaeschna victoria", "Acanthagrion adustum"),
        x = 1:2
    )
    out <- add_critterframe_render_paths(data, dir)

    expect_named(out, c("species", "x", "render_path", "render_id"))
    expect_equal(out$species, data$species)
    expect_equal(
        basename(out$render_path),
        c("Acanthaeschna_victoria__4982372807.png", "Acanthagrion_adustum__5759580977.png")
    )
    expect_true(all(file.exists(out$render_path)))
    expect_identical(out$render_id, c("4982372807", "5759580977"))
})

test_that("add_critterframe_render_paths() gives NA to rows with no render", {
    dir <- render_dir(c("A_alba__1.png", "notes.txt"))
    data <- tibble::tibble(species = c("A alba", "B nigra", NA))
    out <- add_critterframe_render_paths(data, dir)

    expect_equal(nrow(out), 3)
    expect_equal(is.na(out$render_path), c(FALSE, TRUE, TRUE))
    expect_equal(out$render_id, c("1", NA, NA))
})

test_that("add_critterframe_render_paths() takes the group column as an argument", {
    dir <- render_dir(c("Aeshnidae__10.png", "Libellulidae__20.png"))
    data <- tibble::tibble(family = c("Libellulidae", "Aeshnidae"))
    out <- add_critterframe_render_paths(data, dir, by = family)

    expect_equal(out$render_id, c("20", "10"))
})

test_that("add_critterframe_render_paths() uses the first of several renders, with a warning", {
    dir <- render_dir(c("A_alba__200.png", "A_alba__100.png", "B_nigra__300.png"))
    data <- tibble::tibble(species = c("B nigra", "A alba"))

    expect_warning(
        out <- add_critterframe_render_paths(data, dir),
        "1 group has more than one render"
    )
    expect_equal(out$render_id, c("300", "100"))
})

test_that("add_critterframe_render_paths() does not warn about groups absent from the data", {
    dir <- render_dir(c("A_alba__200.png", "A_alba__100.png", "B_nigra__300.png"))
    data <- tibble::tibble(species = "B nigra")

    expect_no_warning(add_critterframe_render_paths(data, dir))
})
