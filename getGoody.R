##----------------------
## This function takes one marked iv curve and summarize its characteristics.
##----------------------
getGoody <- function(oneiv,
                     sens = 3,
                     plot = TRUE)
{
    
#########################
### Part one for bad curves 
#########################
    if(oneiv$type[1] %in% c("few.points", "small.amps", "unknown")){
        ## attach(oneiv)
        ans <- data.frame(date = oneiv$date[1], sample = oneiv$sample[1],
                          temp_bf = oneiv$temp_before[1], temp_aft = oneiv$temp_after[1],
                          irr_bf = oneiv$irr_before[1], irr_aft = oneiv$irr_after[1],
                          I0 = NA, I0.std = NA,
                          I0.p = NA, I.slope = NA,
                          Is.std = NA, Is.p = NA,
                          V0 = NA, V.slope = NA, Vs.std = NA, Vs.p = NA,
                          pmax = max(oneiv$volts * oneiv$amps),
                          pmaxV = oneiv$volts[which.max(oneiv$volts * oneiv$amps)],
                          rsq.Islope = NA, rsq.Vslope = NA,
                          type = oneiv$type[1], cpID = 0, cp = NA, stringsAsFactors = FALSE, row.names = NULL)
        ## detach(oneiv)
        return(ans)
        
#########################
### Part two for good curves
#########################
    }else if(oneiv$type[1] %in% c("I", "II", "III")){
        V0 <- max(oneiv$volts)
        changePts <- oneiv$cp[1]
        changePts <- as.numeric(strsplit(changePts, ',')[[1]])
        vs <- c(changePts, V0)
        changePts <- c(0, changePts)
        I.rsq <- Is.p <- Is.std <- I.slope <- I0.p <- I0.std <- I0 <- rep(NA, length(changePts))
        
        for(i in 1:length(changePts)){
            lmA <- vector("list", 3)
            alphas <- c(0.3, 0.4, 0.5, 0.6)
            rsqs <- rep(NA, length(alphas))
            for(k in 1:length(alphas)){
                theData <- subset(oneiv, volts > 0 & volts >= changePts[i] &
                                      volts <= changePts[i] + alphas[k] * (vs[i] - changePts[i]))
                if(nrow(theData) < 5){
                    rsqs[k] <- -Inf
                }else{
                    lmA[[k]] <- lm(amps ~ volts, data = theData)
                    rsqs[k] <- summary(lmA[[k]])$r.squared
                }
            }
            bestLm <- lmA[[which.max(rsqs)]]
            if(is.null(bestLm) || nrow(summary(bestLm)$coef) < 2){
                I0[i] <- max(oneiv$amps)
                I0.std[i] <- NA
                I0.p[i] <- NA
                I.slope[i] <- NA
                Is.std[i] <- NA
                Is.p[i] <- NA
                I.rsq[i] <- NA
            }else{
                I0[i] <- predict(bestLm, newdata = data.frame(volts = changePts[i]))
                I0.std[i] <- summary(bestLm)$coef[1,2]
                I0.p[i] <- summary(bestLm)$coef[1,4]
                I.slope[i] <- bestLm$coef[2]
                Is.std[i] <- summary(bestLm)$coef[2,2]
                Is.p[i] <- summary(bestLm)$coef[2,4]
                I.rsq[i] <- summary(bestLm)$r.squared
            }
        }
        lmV <- lm(amps ~ -1 + I(volts - V0) + I((volts - V0) ^2) +
                      I((volts - V0) ^3), data = oneiv[(nrow(oneiv) - 15):nrow(oneiv),])
        if(nrow(summary(lmV)$coef) < 2){
            V.slope <- NA
            Vs.std <- NA
            Vs.p <- NA
        }else{
            V.slope <- lmV$coef[1]
            Vs.std <- summary(lmV)$coef[1, 2]
            Vs.p <- summary(lmV)$coef[1, 4]
        }



        ans <- data.frame(date = oneiv$date[1], sample = oneiv$sample[1],
                          temp_bf = oneiv$temp_before[1], temp_aft = oneiv$temp_after[1],
                          irr_bf = oneiv$irr_before[1], irr_aft = oneiv$irr_after[1],
                          I0 = I0, I0.std = I0.std,
                          I0.p = I0.p, I.slope = I.slope,
                          Is.std = Is.std, Is.p = Is.p,
                          V0 = V0, V.slope = V.slope, Vs.std = Vs.std, Vs.p = Vs.p,
                          pmax = max(oneiv$volts * oneiv$amps), pmaxV = oneiv$volts[which.max(oneiv$volts * oneiv$amps)],
                          rsq.Islope = I.rsq, rsq.Vslope = summary(lmV)$r.squared,
                          type = oneiv$type[1], cpID = 0:(length(changePts) -1), cp = changePts,
                          stringsAsFactors = FALSE, row.names = NULL)
        rownames(ans) <- NULL
        return(ans)
    }
}

