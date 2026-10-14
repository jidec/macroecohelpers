# Function to download images from a vector of URLs to a specified folder
downloadImgsFromURLs <- function(urls, download_folder) {
    # Ensure the specified folder exists, create it if not
    if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
    }

    # Loop through each URL in the vector
    for (url in urls) {
        random_string <- stri_rand_strings(n = 1, length = 10, pattern = "[A-Za-z0-9]")

        # Extract the image filename from the URL
        img_filename <- basename(url)
        img_filename <- paste0(random_string,".jpg")

        # Construct the full path for saving the image
        img_filepath <- file.path(download_folder, img_filename)

        # Download the image
        download.file(url, img_filepath, mode = "wb")
    }

    message("Download complete.")
}
