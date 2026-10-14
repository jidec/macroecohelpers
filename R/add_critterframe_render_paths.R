#' Add the paths of critterframe renders that match each row
#'
#' critterframe writes its renders as `{group_name}__{occurrence_id}.png`,
#' where the group is usually a species, such as
#' `Acanthagrion_adustum__5759580977.png`. This matches each row of `df` to
#' the render of its group and adds the render's path and occurrence ID.
#'
#' Underscores in the group part of a filename are read as spaces, so
#' `Acanthagrion_adustum` matches `"Acanthagrion adustum"` in `df`. When a
#' group has more than one render, the first by filename is used, with a
#' warning.
#'
#' @param df A data frame.
#' @param dir Folder holding the renders.
#' @param by <[`data-masking`][dplyr::dplyr_data_masking]> Column of `df`
#'   holding the group name to match, usually the species.
#'
#' @returns `df` with two added character columns: `render_path`, the full
#'   path of the render, and `render_id`, its occurrence ID. Rows with no
#'   render get `NA`.
#'
#' @examples
#' # A folder of renders
#' dir <- tempfile()
#' dir.create(dir)
#' file.create(file.path(dir, c(
#'     "Acanthagrion_adustum__5759580977.png",
#'     "Acanthaeschna_victoria__4982372807.png"
#' )))
#'
#' traits <- tibble::tibble(
#'     species = c("Acanthagrion adustum", "Acanthaeschna victoria", "Aeshna grandis"),
#'     wing_length = c(21.5, 30.4, 47.2)
#' )
#'
#' add_critterframe_render_paths(traits, dir, by = species)
#' @export
add_critterframe_render_paths <- function(df, dir, by = species) {
    # Renders are named {group_name}__{occurrence_id}.png
    paths <- list.files(dir, pattern = "\\.png$", full.names = TRUE)
    stems <- sub("\\.png$", "", basename(paths))
    # Group names are written with underscores for spaces
    groups <- gsub("_", " ", sub("__.*$", "", stems))
    ids <- sub("^.*__", "", stems)

    # Warn when a group in the data has more than one render
    wanted <- dplyr::pull(df, {{ by }})
    repeated <- intersect(groups[duplicated(groups)], wanted)
    if (length(repeated) > 0) {
        cli::cli_warn(
            "{length(repeated)} group{?s} ha{?s/ve} more than one render; using the first."
        )
    }

    # First render of each row's group; rows with no render get NA
    first <- match(wanted, groups)
    df$render_path <- paths[first]
    df$render_id <- ids[first]
    df
}
