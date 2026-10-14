filterNorthAmerica <- function(df) {
    # Define the latitude and longitude bounds for North America
    lat_min <- 5
    lat_max <- 85
    lon_min <- -168
    lon_max <- -52

    # Filter the dataframe based on these bounds
    df_filtered <- df %>%
        filter(latitude >= lat_min & latitude <= lat_max &
                   longitude >= lon_min & longitude <= lon_max)

    return(df_filtered)
}
