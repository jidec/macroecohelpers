
readDarwincoreDF <- function(dwc_zip_file){
    library(finch)
    dwc <- dwca_read(dwc_zip_file, read = TRUE)
    dwc_data <- as.data.frame(dwc$data[[1]])
    return(dwc_data)
}
