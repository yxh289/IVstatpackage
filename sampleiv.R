sampleiv <- function (dat, n = 10){
  unqtime <- unique(dat$time)
  if(length(unqtime) > n){
    unqtime <- sample(unqtime, n)
    dat <- subset(dat, dat$time %in% unqtime)
  }
  else{}
  return(dat) 
}