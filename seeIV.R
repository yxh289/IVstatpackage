seeIV <- function(mydate, mytime, mysample, dat.m, dat.s){
    oneivMarked <- subset(dat.m, date == as.character(mydate) & time == as.character(mytime) & sample == as.character(mysample))
    oneivSum <- subset(dat.s, date == as.character(mydate) & time == as.character(mytime) & sample == as.character(mysample))

    a <- ggplot() + geom_point(data = oneivMarked, aes(x = volts, y = amps), alpha = 0.5)
    cpSt <- oneivSum$cp
    cpEnd <- c(cpSt, oneivSum$V0[1])[-1]
    cpI <- oneivSum$I0
    Islope <- oneivSum$I.slope
    lineSegs <- data.frame(x = cpSt, y = cpI, xend = cpSt + 0.8 * (cpEnd - cpSt),
                           yend = cpI + Islope * 0.8 * (cpEnd - cpSt))
    for(i in 1:length(cpSt)){
        a <- a + geom_segment(data = lineSegs, aes(x = x, y = y, xend = xend, yend = yend), col = "red", size = 0.7)
        if(i > 1){
            a <- a + geom_text(data = lineSegs, aes(x = x, y = y, label = round(x, 2)), hjust = -0.3, vjust = -0.3)
            a <- a + geom_vline(xintercept = cpSt[i], colour = "skyblue", linetype = 2)
        }
    }
    a <- a + labs(title = paste(mydate, "-", mytime, "-", mysample), x = "Volts", y = "Amps")
    return(a)
}
