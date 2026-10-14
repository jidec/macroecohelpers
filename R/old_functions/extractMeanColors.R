

extractMeanColors <- function(img_dir, start_index=1){
    library(imager)
    library(sqldf)
    library(dplyr)

    # get paths
    paths <- list.files(img_dir, full.names = TRUE)
    paths <- paths[start_index:length(paths)]

    # loop through
    df <- data.frame()
    i <- 0
    for(path in paths){
        if(i %% 100 == 0){
            print(i)
        }
        i <- i + 1

        if(file.exists(path) & endsWith(path,".png")){
            img <- load.image(path)
            arr <- as.array(img)

            # reshape to pixels
            dim(arr) <- c(dim(arr)[1] * dim(arr)[2],3)
            #dim(arr) <- c(dim(arr)[1] * dim(arr)[2],4)

            # remove black background pixels
            arr <- arr[(arr != c(0,0,0))[,1],]
            #arr <- arr[(arr != c(0,0,0,0))[,1],]

            name <- strsplit(path, "/", fixed = TRUE)[[1]]
            name <- name[length(name)]
            name <- strsplit(name, "_", fixed = TRUE)[[1]]
            name <- name[1]

            # create row for image
            row <- c(name)

            # add mean color to row
            mean_rgb <- c(mean(arr[,1]), mean(arr[,2]), mean(arr[,3]))
            row <- c(row,mean_rgb)
            #print(row)
            df <- rbind(df,row)
        }
    }

    colnames(df) <- c("recordID","mean_r", "mean_g","mean_b")

    return(df)
}
