# d must have "annotations" column found in iNat records
bindiNatSexAnnotation <- function(d){
    library(stringr)
    d$sex <- case_when(
        str_detect(d$annotations, "Male") ~ "male",
        str_detect(d$annotations, "Female") ~ "female",
        TRUE ~ "unknown"
    )
    return(d)
}
