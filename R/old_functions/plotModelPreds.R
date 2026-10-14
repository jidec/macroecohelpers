plotModelPreds <- function(model,terms=list())
{
    for(t in terms){
        plot(plot_model(model,type="pred",terms=t))
    }
    #dotplot(ranef(model,condVar=T))
}
