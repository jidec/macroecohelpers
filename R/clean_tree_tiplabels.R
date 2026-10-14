#' Clean the tip labels of a phylogeny so they match species names in data
#'
#' Tip labels in tree files are often written like `'Papilio_glaucus'`. This
#' replaces underscores with spaces and removes quote marks, so the labels
#' read like ordinary species names.
#'
#' @param tree A `phylo` object, such as one read by [ape::read.tree()].
#' @param drop_first_word If `TRUE`, also remove the first word of each
#'   label, for labels that start with a family name or an ID, such as
#'   `Papilionidae_Papilio_glaucus`.
#'
#' @returns `tree` with its tip labels changed. The tips and their order are
#'   untouched.
#'
#' @examplesIf rlang::is_installed("ape")
#' tree <- ape::read.tree(text = "((Papilio_glaucus,Papilio_troilus),Pieris_rapae);")
#' clean_tree_tiplabels(tree)$tip.label
#'
#' tree <- ape::read.tree(text = "((Papilionidae_Papilio_glaucus,Pieridae_Pieris_rapae));")
#' clean_tree_tiplabels(tree, drop_first_word = TRUE)$tip.label
#' @export
clean_tree_tiplabels <- function(tree, drop_first_word = FALSE) {
    labels <- tree$tip.label

    # Underscores to spaces
    labels <- gsub("_", " ", labels, fixed = TRUE)
    # Remove quote marks
    labels <- gsub("['\"]", "", labels)
    # Remove a leading family name or ID
    if (drop_first_word) labels <- sub("^\\s*\\S+\\s+", "", labels)

    tree$tip.label <- trimws(labels)
    tree
}
