vizID <- function(ids,repo_path="E:/dragonfly-patterner",
                        viz_image=TRUE, img_suffix_ext =".jpg",
                        viz_segment=F, seg_suffix_ext ="_fore_segment.png",
                        viz_pattern_folder=NULL, pat_suffix_ext ="_fore_pattern.png"){

    img_list <- list()
    for(id in ids){
        library(imager)
        if(viz_image){
            path <- paste0(repo_path,'/data/all_images/',id,img_suffix_ext)
            img <- load.image(path)
            img_list <- append(img_list,list(img))
            #plot(img)
        }
        if(viz_segment){
            path <- paste0(repo_path,'/data/segments/',id,seg_suffix_ext)
            img <- load.image(path)
            img_list <- append(img_list,list(img))
            #plot(img)
        }
        if(!is.null(viz_pattern_folder)){
            path <- paste0(repo_path,'/data/patterns/',viz_pattern_folder,'/',id,pat_suffix_ext)
            img <- load.image(path)
            img_list <- append(img_list,list(img))
            #plot(img)
        }
    }

    par(mar=c(0,0,0,0))
    plot(as.imlist(img_list),cex=0.1)
}

#vizID("INAT-49781305-4",repo_path = "E:/dragonfly-patterner")

#vizID(id="WRK-WS-02580",repo_path="D:/wing-color/",viz_image = F,viz_segment = T, viz_pattern_folder = "grouped")

