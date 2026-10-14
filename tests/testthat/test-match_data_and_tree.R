tree <- ape::read.tree(text = "((A_alba,B_nigra),(C_rubra,D_viridis));") |>
    clean_tree_tiplabels()

test_that("match_data_and_tree() drops unmatched rows and tips", {
    data <- tibble::tibble(species = c("A alba", "C rubra", "E flava"), x = 1:3)
    out <- match_data_and_tree(data, tree)

    expect_named(out, c("data", "tree"))
    expect_equal(out$data, tibble::tibble(species = c("A alba", "C rubra"), x = 1:2))
    expect_s3_class(out$tree, "phylo")
    expect_equal(out$tree$tip.label, c("A alba", "C rubra"))
})

test_that("match_data_and_tree() keeps every row of a matched species", {
    data <- tibble::tibble(species = c("B nigra", "A alba", "B nigra", "B nigra"), x = 1:4)
    out <- match_data_and_tree(data, tree)

    expect_equal(out$data, data)
    expect_equal(out$tree$tip.label, c("A alba", "B nigra"))
})

test_that("match_data_and_tree() keeps row and tip order", {
    data <- tibble::tibble(species = c("D viridis", "E flava", "A alba", "C rubra"))
    out <- match_data_and_tree(data, tree)

    expect_equal(out$data$species, c("D viridis", "A alba", "C rubra"))
    expect_equal(out$tree$tip.label, c("A alba", "C rubra", "D viridis"))
})

test_that("match_data_and_tree() takes the species column as an argument", {
    data <- tibble::tibble(taxon = c("A alba", "E flava", "D viridis"))
    out <- match_data_and_tree(data, tree, species = taxon)

    expect_equal(out$data$taxon, c("A alba", "D viridis"))
    expect_equal(out$tree$tip.label, c("A alba", "D viridis"))
})

test_that("match_data_and_tree() errors when nothing matches", {
    data <- tibble::tibble(species = c("A_alba", "B_nigra"))

    expect_error(match_data_and_tree(data, tree), "clean_tree_tiplabels")
})
