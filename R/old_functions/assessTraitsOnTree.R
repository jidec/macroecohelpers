#' Plot phytools contMaps of one or two traits, their phylogenetic signature,
#' their phylo correlation, and their diversification rate
#'
#' @description Load a phylo tree, trim the df to the tree,
#' match the order of the tips to the trait, plot contMap and compute phylo sig
#' @param df df containing trait
#' @param trait_colname colname for the trait
#' @param tree_location file loc of the tree (read using ape::read.tree)
#' @param replace_underscores whether underscores should be replaced with spaces in the loaded tree
#'
#' @return nothing

#df = wings_sp
#tree = loadFixTree("data/dragonfly_tree.tre")
#trait_colname = "blackbrown"
#trait_colname2 = "brownyellow"
#trait_colname = "bb_present"
#trait_colname2 = "by_present"
#legend = TRUE

# REMEMBER that you must summarize the df to the species, genus etc. beforehand
assessTraitsOnTree <- function(df, tree, trait_colname,trait_colname2=NULL,legend=TRUE){
    library(ape)
    library(phytools)
    library(dplyr)
    library(caper)

    trimmed_tuple <- trimDfToTree(df,tree)

    df <- trimmed_tuple[[1]]
    tree <- trimmed_tuple[[2]]

    t1map <- plotOnTree(df,tree,trait_colname,legend)
    if(!is.null(trait_colname2)){
        t2map <- plotOnTree(df,tree,trait_colname2,legend)
        par(mfrow=c(1,2))
        plot(t1map,ftype="off",lwd=2)# rm leg

        plot(t2map,direction="leftwards",ftype="off",lwd=2)

        if(TRUE){ #class(t[,1]) == "numeric" & class(t[,2]) == "numeric"
            t <- cbind(df[,trait_colname],df[,trait_colname2])
            rownames(t) <- df$clade
            t <- as.matrix(t)

            library(nlme)
            # APPR -3
            #fit.yx<-gls(blackbrown~brownyellow,data=as.data.frame(t),correlation=corBrownian(1,tree))
            fit.yx <- eval(parse(text=paste0("gls(",trait_colname,"~",trait_colname2,", data=as.data.frame(t), correlation=corBrownian(1,tree))")))
            #fit.yx<-gls(bb_present~by_present,data=as.data.frame(t),correlation=corBrownian(1,tree))
            print(anova(fit.yx))
            print(r2(fit.yx))

            # APRR -2
            #obj<-phyl.vcv(t,vcv(tree),1)
            ## correlation between x & y
            #r.xy<-cov2cor(obj$R)[trait_colname,trait_colname2]
            ## t-statistic & P-value
            #t.xy<-r.xy*sqrt((Ntip(tree)-2)/(1-r.xy^2))
            #P.xy<-2*pt(abs(t.xy),df=Ntip(tree)-2,lower.tail=F)
            #print(P.xy)

            # APPR -1 LOL
            # Assuming your data frame is named `traits_data` and has columns `trait1` and `trait2`
            #pic_trait1 <- pic(t[,2], tree)
            #pic_trait2 <- pic(t[,1], tree)

            #print(cor.test(pic_trait1, pic_trait2))

            # APPR 0

            # Prepare your comparative data
            #comp_data <- comparative.data(phy=tree, data=df, names.col="clade")

            # Fit a PGLS model assessing the correlation between two traits
            #model <- pgls(blackbrown ~ brownyellow, data=comp_data, lambda="ML")

            # Display the summary of the model
            #print(summary(model))

            # APPR 1
            #library(nlme)
            #spp <- rownames(t)

            # run pgls for cor between traits
            #m <- eval(parse(text=paste0("gls(",trait_colname,"~",trait_colname2,", data = df, correlation=corBrownian(phy=tree, form=~clade))")))
            #print(summary(m))

            # APPR 2
            # Calculate the phylogenetic correlation matrix
            #v <- vcv(tree,  model = "Brownian", corr = TRUE)

            # Fit the GLS model using the lm.gls() function
            #z <- lm.gls(x = df[trait_colname], y = df[trait_colname2], v = v)

            #print(summary(z$lm.fit))
            #print(anova(z$lm.fit))
        }
    }
}

# returns the trait, either discrete or continuous, modeled on the phylo
plotOnTree <- function(df,tree,trait_colname,legend){

    # prep traits for asr
    trait <- as.vector(df[,trait_colname]) #change this to meanMD, meanHour, propNight etc
    trait <- unlist(trait)
    names(trait) <- df$clade

    # unlist tree tips
    tree$tip.label <- unlist(tree$tip.label)

    #makes trait indices match order of tree tips
    matchOrder <- function(tree, traitvect)
    {
        ordered <- traitvect
        for(i in 1:length(tree$tip.label))
        {
            tipstr <- tree$tip.label[i]
            for(ii in 1:length(traitvect))
            {
                if(tipstr == names(traitvect[ii]))
                {
                    ordered[i] <- traitvect[ii]
                    names(ordered)[i] <- tipstr
                }
            }
        }
        return(ordered)
    }

    trait <- matchOrder(tree, trait)

    # replace NaN edge lengths with 0
    tree$edge.length <- ifelse(is.nan(tree$edge.length),0, tree$edge.length)
    tree$edge.length[tree$edge.length == 0] <- 0.1

    # if numeric
    if(class(trait) == "numeric"){
        # create contMap for all genera
        obj <- contMap(tree,trait,method="anc.ML",fsize = 0,legend=legend)#,lims=c(-0.15,0.4874))
        #errorbar.contMap(obj)

        obj<-setMap(obj,colors=c("grey","black"))
        cex <- 1
        #if(!tipnames){
        #    cex <- 0
        #}
        plot(obj,type="fan",legend=0.7*max(nodeHeights(tree)), fsize=c(0.6,0.9),cex=0.5,show.tip.label=FALSE)

        # compute phylogenetic signal
        physig <- phylosig(tree, trait, method="K", test=TRUE, nsim=1000, se=NULL, start=NULL,
                           control=list())
        physigL <- phylosig(tree, trait, method="lambda", test=FALSE, nsim=1000, se=NULL, start=NULL,
                            control=list())
        print(physig)
        plot(physig,las=1,cex.axis=0.9)
        print(physigL)

        return(obj)
    }
    # if factor
    else{
        smap.trees <- make.simmap(tree,trait,model="ER",
                                  nsim=10)
        cols<-setNames(c("green","#E4D96F","darkgreen"),
                       levels(trait))

        plot(smap.trees[[1]],cols,fsize=0.5,ftype="i",outline=TRUE)
        add.simmap.legend(colors=cols,prompt=FALSE,x=0,y=-0.5,
                          vertical=FALSE)

        print(describe.simmap(smap.trees))
        return(smap.trees[[1]])
    }
}
