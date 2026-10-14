
# Define the bindBioclimData function
bindBioclimData <- function(df, bioclim_var_num=1, new_var_colname="tmax", res = 10, path = 'D:/bioclim_data') {

    library(geodata)
    library(terra)
    library(dplyr)

    # Download Bioclim data for the specified resolution
    bioclim_data <- worldclim_global(var = "bio", res = res, path = path)

    bioclim_layer <- bioclim_data[[bioclim_var_num]]

    # Convert the dataframe to a SpatVector
    points <- vect(df, geom = c("longitude", "latitude"), crs = crs(bioclim_layer))

    # Extract Bioclim data for the given points
    extracted_data <- extract(bioclim_layer, points)

    # Combine the original dataframe with the Bioclim data
    result_df <- cbind(df, extracted_data[, -1])  # Remove the ID column from the extraction results

    # rename last column
    new_name <- new_var_colname
    last_col_name <- names(result_df)[ncol(result_df)]
    result_df <- result_df %>% rename(!!new_name := all_of(last_col_name))

    return(result_df)
}
