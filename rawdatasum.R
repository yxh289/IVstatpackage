##create data summary table
## 03/04/2015 Yang Hu
rawdatasum <- function(work_directry){
  files <- list.files(path = work_directry, pattern = "^IVs2.*\\.csv$")
  datsum <- data.frame(NULL)
  for ( i in 1: length(files)){
    #browser()
    dat <- read.csv(file = paste(work_directry,"/",files[i],sep ="" ), header = T)
    #dat <- dat[dat$sample %in% c( "sa18145.00","sa18228.00","sa18148.00" ),]
    if(!dim(dat)[1]==0){
      smallivdata <- group_by(dat, sample, date)
      #smallivdata <- do(smallivdata, sampleiv(.))
      datsubset_g <- group_by(smallivdata ,sample, date, time)
      datsum_g <- summarise(datsubset_g)
      datsum_g <- summarise(datsum_g, count = n())
      datsum <-rbind(datsum_g,datsum)
    }
    #print(files[i])
  }
  return(datsum)
}