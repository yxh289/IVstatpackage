##--------------------
## Visualize all IV curves from one day
##--------------------
see <- function(oneday, ...){
    gplot <- ggplot(oneday, aes(x = volts, y = amps, group = time), ...) +
        geom_point(size = 0.6, ...) + ggtitle(oneday$date[1], ...)
    if(length(levels(oneday$sample)) > 1){
        gplot <- gplot + facet_wrap(~ sample, ncol = 4, ...) 
    }
    plot(gplot)
}
