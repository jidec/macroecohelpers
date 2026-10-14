# remove individuals such that for each species, the df has the same number of males and females
# sex must be capitalized
setSpSexesEqual <- function(df){
    out <- data.frame()
    for(sp in unique(df$species)){
        sp_m <- filter(df,species==sp & Sex == "M")
        sp_f <- filter(df,species==sp & Sex == "F")

        n_m <- nrow(sp_m)
        n_f <- nrow(sp_f)
        diff <- abs(n_m - n_f)

        if(n_m > n_f){
            sp_m <- sample_n(sp_m,n_m - diff)
        }
        if(n_f > n_m){
            sp_f <- sample_n(sp_f,n_f - diff)
        }
        out <- rbind(out,sp_m)
        out <- rbind(out,sp_f)
    }
    return(out)
}
