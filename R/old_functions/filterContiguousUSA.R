filterContiguousUSA <- function(df){
  # Define the bounding box for the contiguous USA
  min_latitude <- 24.396308
  max_latitude <- 49.384358
  min_longitude <- -125.001651
  max_longitude <- -66.93457

  # Filter the dataframe for rows within the bounding box
  df <- df %>%
    filter(latitude >= min_latitude, latitude <= max_latitude,
             longitude >= min_longitude, longitude <= max_longitude)

  return(df)
}
