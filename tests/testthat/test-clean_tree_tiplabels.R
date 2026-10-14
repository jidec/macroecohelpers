test_that("clean_tree_tiplabels() replaces every underscore with a space", {
    tree <- ape::read.tree(text = "((Papilio_glaucus,Papilio_troilus_ilioneus),Pieris_rapae);")
    out <- clean_tree_tiplabels(tree)

    expect_equal(
        out$tip.label,
        c("Papilio glaucus", "Papilio troilus ilioneus", "Pieris rapae")
    )
})

test_that("clean_tree_tiplabels() removes quote marks", {
    tree <- ape::read.tree(text = "(A_alba,B_nigra);")
    tree$tip.label <- c("'A_alba'", "\"B_nigra\"")

    expect_equal(clean_tree_tiplabels(tree)$tip.label, c("A alba", "B nigra"))
})

test_that("clean_tree_tiplabels() drops the first word when asked", {
    tree <- ape::read.tree(text = "(Papilionidae_Papilio_glaucus,Pieridae_Pieris_rapae);")
    out <- clean_tree_tiplabels(tree, drop_first_word = TRUE)

    expect_equal(out$tip.label, c("Papilio glaucus", "Pieris rapae"))
})

test_that("clean_tree_tiplabels() changes nothing but the tip labels", {
    tree <- ape::read.tree(text = "((A_alba:1,B_nigra:2):1,C_rubra:3);")
    out <- clean_tree_tiplabels(tree)

    expect_s3_class(out, "phylo")
    expect_equal(out$edge, tree$edge)
    expect_equal(out$edge.length, tree$edge.length)
})
