library(png)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)


# Function to create a data frame with color counts for each image
dfFromBCCPatternFolder <- function(img_folder) {

    # Function to convert image to a list of colors
    flattenImageColors <- function(image) {
        # Convert the image matrix to a list of colors (as hex codes)
        hex_colors <- rgb(image[,,1], image[,,2], image[,,3], maxColorValue = 1)
        return(hex_colors)
    }

    # Function to get all unique colors from all images
    getAllUniqueColors <- function(image_files) {
        unique_colors <- vector("list", length(image_files))

        for (i in seq_along(image_files)) {
            image <- readPNG(image_files[i])
            unique_colors[[i]] <- unique(flattenImageColors(image))
        }

        unique_colors <- unique(unlist(unique_colors))
        return(unique_colors)
    }

    # get names of images in folder
    image_files <- list.files(img_folder, full.names = TRUE, pattern = "\\.png$")

    # get unique colors
    unique_colors <- getAllUniqueColors(image_files)

    color_counts_list <- vector("list", length(image_files))
    names(color_counts_list) <- image_files

    for (file in image_files) {
        image <- readPNG(file)
        flat_colors <- flattenImageColors(image)
        color_counts <- table(factor(flat_colors, levels = unique_colors))
        color_counts_list[[file]] <- as.data.frame(t(color_counts))
    }
    color_df <- bind_rows(color_counts_list, .id = "image")

    # remove extraneous col
    color_df <- color_df %>% select(-Var1)
    # convert image path to id
    color_df$image <- str_split_fixed(basename(color_df$image),"_",2)[,1]
    # rename cols
    colnames(color_df) <- c("img_id","color","n_pixels")
    # make numeric
    color_df$n_pixels <- as.numeric(color_df$n_pixels)
    # we now have a df to spread

    # First, calculate the total number of pixels per image_id
    total_pixels <- color_df %>%
        group_by(img_id) %>%
        summarise(total_n = sum(n_pixels)) %>%
        ungroup()

    # Next, prepare the data for wide transformation by creating a color ranking
    color_df <- color_df %>%
        group_by(img_id) %>%
        mutate(color_rank = paste0('color_', row_number())) %>%
        ungroup()

    # Now, spread the colors and their pixel counts to wide format
    wide_df <- color_df %>%
        select(-color) %>%
        spread(color_rank, n_pixels) %>%
        group_by(img_id) %>%
        summarise_all(mean,na.rm=TRUE)

    colnames(wide_df) <- c("img_id", as.character(unique(color_df$color)))

    wide_df <- wide_df %>%
        mutate(total_sum = rowSums(select(., -1))) %>%
        select(img_id, total_sum, everything())

    names(wide_df) <- gsub("#", "", names(wide_df))
    names(wide_df) <- paste0("color_", names(wide_df))

    return(wide_df)
}

