filter_curves <- function(oneiv, sens = 3, plot = FALSE){  
    if(nrow(oneiv) < 50) return(data.frame(type = "few.points", cp = NA))
    else if(max(oneiv$amps) < 1.1) return(data.frame(type = "small.amps", cp = NA))
    ##     else return(data.frame(type = "regular"))
    else{
        ctr <- loess.control(surface = "interpolate",
                             statistics = "approximate",
                             trace.hat = "approximate")
        res <- loess(amps ~ volts, span = 0.05, degree = 1,
                     data = oneiv, control = ctr)
        oneiv$resid <- residuals(res)
        
        cpts <- which(abs(residuals(res)) > sens * (summary(res)$s))
        midSection <- with(
            oneiv, which(volts < 0.90 * max(volts) &
                             volts > quantile(volts, 0.1)))
        cpts <- cpts[cpts %in% midSection]
        if(length(cpts) > 0){
            unqcp <- NULL
            ## Find unique change points. 
            i <- 1
            while(length(cpts) > 0){
                themax <- which.max(-(residuals(res)[cpts]))
                unqcp[i] <- oneiv[cpts[themax], "volts"]
                cpts <- cpts[!(cpts %in% (cpts[themax] - 5):(cpts[themax] + 5))]
                i <- i + 1
            }
            cp <- paste(unqcp, collapse = ",")
            if(length(unqcp) == 1) type <- "II"
            else if(length(unqcp) == 2) type <- "III"
            else type <- "unknown"
        }else{
            type <- "I"
            cp <- "0"
        }

#########################
###  Plot (does not work; need to modify)
#########################
        if(plot == TRUE){
            aa <- ggplot(oneiv, aes(x = volts, y = amps, group = time)) +
                geom_point(alpha = 0.5)
            aa <- aa + geom_line(aes(x = volts, y = pred), color = "red", alpha = 0.5)
            aa <- aa + geom_point(aes(x = volts,
                                      y = 20 * resid), color = "blue", alpha = 0.5)
            aa <- aa + geom_hline(yintercept = 20 * sens * summary(res)$s,
                                  color = "gray")
            aa <- aa + geom_hline(yintercept = -20 * sens * summary(res)$s,
                                  color = "gray")
            filename <- paste0(substring(onefile, 1, 13),
                               ".", oneiv$time[1], ".", oneiv$sample[1])
            aa <- aa + ggtitle(filename)
            if(ans$type == "bad"){
                aa <- aa + geom_vline(xintercept = unlist(ans$cp),
                                      color = "red", linetype = "longdash", size = 0.5)
            }
            ggsave(filename = paste0(filename, ".png"), aa, width = 10, height = 5)
        }

        return(data.frame(type = type, cp = cp))
    }
}

mark_curves <- function(oneday){
    onedayg <- group_by(oneday, sample, time)
    marks <- do(onedayg, filter_curves(.))
    ## goodones <- filter(marks, type == "good")
    ## oneday_regular <- inner_join(oneday, filter(marks, type == "regular"))
    oneday_marked <- inner_join(oneday, marks)
}

## mark_curves(oneday)

