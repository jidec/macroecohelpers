#' Keep only the species found in both a data frame and a phylogeny
#'
#' Most comparative methods need the data and the tree to cover exactly the
#' same species. This drops the rows of `data` whose species is not a tip of
#' `tree`, and the tips of `tree` that have no rows in `data`.
#'
#' Rows and tips keep their original order. Methods that need one row per tip,
#' in tip order, still need the data summarised and arranged afterwards.
#'
#' @param data A data frame.
#' @param tree A `phylo` object whose tip labels are written the same way as
#'   the species in `data` (see [clean_tree_tiplabels()]).
#' @param species <[`data-masking`][dplyr::dplyr_data_masking]> Column of
#'   `data` holding the names to match against the tip labels.
#'
#' @returns A list with two elements: `data`, a tibble of the matched rows,
#'   and `tree`, the `phylo` object with unmatched tips dropped.
#'
#' @examples
#' tree <- ape::read.tree(text = "((A_alba,B_nigra),(C_rubra,D_viridis));") |>
#'     clean_tree_tiplabels()
#' traits <- tibble::tibble(
#'     species = c("A alba", "A alba", "C rubra", "E flava"),
#'     wing_length = c(21.5, 22.1, 30.4, 18.9)
#' )
#'
#' matched <- match_data_and_tree(traits, tree)
#' matched$data
#' matched$tree$tip.label
#' @export
match_data_and_tree <- function(data, tree, species = species) {
    # Species present in both the data and the tree
    shared <- intersect(tree$tip.label, dplyr::pull(data, {{ species }}))
    if (length(shared) == 0) {
        cli::cli_abort(c(
            "No values of {.arg species} match the tip labels of {.arg tree}.",
            i = "Tip labels may need {.fn clean_tree_tiplabels} first."
        ))
    }

    list(
        # Rows whose species is in the tree
        data = dplyr::filter(data, {{ species }} %in% shared) |> tibble::as_tibble(),
        # Tips that have data
        tree = ape::keep.tip(tree, shared)
    )
}
