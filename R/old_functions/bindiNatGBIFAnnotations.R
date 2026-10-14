# data must have occurrenceID column
bindiNatGBIFAnnotations <- function(data, annotation_colname, darwincore_zip_location){
    library(finch)
    library(dplyr)
    dwc <- dwca_read(darwincore_zip_location, read = TRUE)
    dwc_data <- as.data.frame(dwc$data[[1]])
    dwc_data <- dwc_data[,c("occurrenceID", annotation_colname)]

    data <- merge(data,dwc_data,by="occurrenceID")
    return(data)
}
