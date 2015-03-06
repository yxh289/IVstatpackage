# server.R
# v0.00.04-Yang HU Mar. 
# v0.00.03-Yang Hu Mar. 1 2015
# v0.00.02-Yang Hu Mar. 1 2015
# v0.00.01-Yang Hu Feb. 26 2015 

library(ggplot2)
library(dplyr)
library(reshape)
source("sampleiv.R")
source("see.R")
source("getGoody.R")
source("mark_curves_funs.R")
source("seeIV.R")
source("rawdatasum.R")
options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output){
  workdir <- reactive({
    input$wdset
    isolate({
    if(input$workdir){
    EDD <- paste0(getwd(),"/data")} 
    else
    {EDD <- input$workdir1}
    return(EDD)
    })
  })
  output$textinput <- renderPrint(workdir())
  rawdatsum <- reactive({
    input$wdset
    isolate({
      a<-rawdatasum(workdir())
      })
    return(a)
  })
  output$ui_sample0 <- renderUI({
  #select sample
  samples <-  unique(rawdatsum()$sample)
  cho <- vector("list",length(samples))
  names(cho) <- samples
  for (i in 1:length(samples)){
    cho[[i]] <- samples[i]
  }
  
  checkboxGroupInput("sample", label = h3("Sample"), 
                     choices = cho,
                     selected = samples) 
  
  })
  output$value1 <- renderPrint({ input$sample })
  output$value2 <- renderPrint({ input$dates })
  output$value3 <- renderPrint({ input$sampleiv })
  output$value4 <- renderPrint({ input$samplenum })
  #data after first page subset->datsubset1
  datsubset1 <- reactive({ 
    #subset to selected sample
    temp <- subset ( rawdatsum(), sample %in% input$sample)
    #subset to selected date
    temp<- subset (temp, (as.POSIXct(date, format = "%m/%d/%Y") >= as.POSIXct(input$dates[1]))&(as.POSIXct(date, format = "%m/%d/%Y") <= as.POSIXct(input$dates[2])))
    # return 
    return(temp)
  })
  
  #histplot for page one
  output$histPlot <- renderPlot({
    
#     #Number of observations each day
#     datsubset_g <- group_by(datsubset1(),sample, date, time)
#     datsum <- summarise(datsubset_g)
#     datsum <- summarise(datsum, count = n())
    
    # You can access the values of the widget (as a vector of Dates)
    # with input$dates,
    ggplot (datsubset1(), aes (x = as.POSIXct(date,format = "%m/%d/%Y"), y = count, fill = sample)) + 
      geom_bar (stat = "identity",position = "stack")+
      ggtitle("Available I-V curves") + xlab( "Time")+ ylab("Number of I-V curves")
    
  })
  ##make a note that data have been imported
  output$nText <- renderText({
    input$import
    isolate("Data have been imported")
  })
  #import data
  dat <- reactive({ 
    input$import
    isolate({
      
      files <- list.files(path = workdir(), pattern = "^IVs2.*\\.csv$")
      samfiles <- files[as.Date(substr(files,4,13)) %in% c (as.Date(input$dates[1]):as.Date(input$dates[2]))]
      cleanivdata <- data.frame(NULL) 
      # progress bar
      withProgress (message = "Parsing file",detail = "names", min = 0, max = length(samfiles) ,{
       for ( i in 1: length(samfiles)){
        dat <- read.csv(file = paste(workdir(),"/", samfiles[i], sep = ""), header = T)
        dat <- dat[dat$sample %in% input$sample,]
        cleanivdata <- rbind(cleanivdata,dat)
      incProgress(amount = 0.1, message = "Parsing file", detail = samfiles[i])
        }
        if(input$sampleiv){
       cleanivdata <- sampleiv (cleanivdata, input$samplenum)  
      }
      
      })
      return(cleanivdata)
    })
  })
  
  #summary table for page two
  sumtable <- reactive({
    #group to each I-V 
    datsubset_g <- group_by(dat(),sample, date, time)
    #count number of points in each I-V
    datpoint <- summarise(datsubset_g, count = n())
    #Avg temp for each I-V
    dattemp <- summarise(datsubset_g, avg = mean(c(unique(temp_before), unique(temp_after))))
    #Avg irr for each I-V
    datirr <- summarise(datsubset_g, avg = mean(c(unique(irr_before),unique(irr_after))))
    #diff irr for each I-V
    datirrdiff <- summarise(datsubset_g, diff = unique(irr_before)-unique(irr_after))
    #make summary table
    sumtable <- data.frame(datpoint$count,dattemp$avg,datirr$avg,datirrdiff$diff)
    return(list(sumtable,datpoint,dattemp,datirr,datirrdiff))
  })
  
  #summary table
  output$sumtable1 <- renderTable({
    
    sumtab <- summary(sumtable()[[1]])
    colnames(sumtab) <- c("Number of data points","Ambient Temperature","Global Horizontal Irradiance","Irradiance Differences")
    return(sumtab)
  })
  #summary box plot
  output$sumboxplot <- renderPlot(boxplot(sumtable()[[1]], outline = FALSE, names = c("Number of data points in each I-V curve","Ambient Temperature","Global Horizontal Irradiance","Irradiance Differences")))
  #dynamic slider
  output$ui_num <- renderUI({
      sliderInput("slider_num", label = h3("Number of data points"), min = min(sumtable()[[1]][,1]), max = max(sumtable()[[1]][,1]), 
                  value = c(min(sumtable()[[1]][,1]),max(sumtable()[[1]][,1])))
    })
  output$ui_temp <- renderUI({
    sliderInput("slider_temp", label = h3("Ambient Temperature(Avg)"), min = min(sumtable()[[1]][,2],na.rm = TRUE), max = max(sumtable()[[1]][,2],na.rm = TRUE), 
                value = c(min(sumtable()[[1]][,2],na.rm = TRUE),max(sumtable()[[1]][,2],na.rm = TRUE)))
   })
  output$ui_irr <- renderUI({
    sliderInput("slider_irr", label = h3("Global Horizontal Irradiance (Avg)"), min = min(sumtable()[[1]][,3],na.rm = TRUE), max = max(sumtable()[[1]][,3],na.rm = TRUE), 
                value = c(min(sumtable()[[1]][,3],na.rm = TRUE),max(sumtable()[[1]][,3],na.rm = TRUE)))
  })
  output$ui_irrdiff <- renderUI({
    sliderInput("slider_irrdiff", label = h3("Irradiance Differences"), min = min(sumtable()[[1]][,4]), max = max(sumtable()[[1]][,4]), 
                value = c(min(sumtable()[[1]][,4]),max(sumtable()[[1]][,4])))
  })
  output$slider_num <- renderPrint({ input$slider_num })
  output$slider_temp <- renderPrint({ input$slider_temp })
  output$slider_irr <- renderPrint({ input$slider_irr })
  output$slider_irrdiff <- renderPrint({ input$slider_irrdiff })
  ##data subset
  subdat1 <- reactive({
    input$subset1
    isolate({
      #browser()
    sub1 <- subset(sumtable()[[2]],(count >= input$slider_num[1])&(count <= input$slider_num[2]))
    sub2 <- subset(sumtable()[[3]],(avg >= input$slider_temp[1])&(avg <= input$slider_temp[2])|(avg == NA))
    if(input$extremetemp){sub2 <- subset(sub2,(avg!=-9999)&(avg!=9999))}
    if(input$NAtemp){sub2 <- sub2[complete.cases(sub2),]}
    sub3 <- subset(sumtable()[[4]],(avg >= input$slider_irr[1])&(avg <= input$slider_irr[2])|(avg == NA))
    colnames(sub3)[4] <- "avgirr"
    if(input$extremeirr){sub3 <- subset(sub3,(avgirr!=-9999)&(avgirr!=9999))}
    sub4 <- subset(sumtable()[[5]],(diff >= input$slider_irrdiff[1])&(diff <= input$slider_irrdiff[2]))
    sub5 <- inner_join(sub1,sub2)
    sub5 <- inner_join(sub5,sub3)
    sub5 <- inner_join(sub5,sub4)
    ##subsetting data
    subdat = data.frame()
    withProgress (message = "Subsetting I-V curves in",detail = "", min = 0, max = length(sub5[,1]) ,{
    #browser()
      for (i in 1 :dim(sub5)[1]){
    temp <- filter(dat(),(sample == sub5$sample[i])&(date == sub5$date[i])&(time == sub5$time[i]))
    subdat <- rbind(subdat,temp)
    incProgress(amount = 0.1, message = "Subsetting I-V curves in", detail = paste(sub5$sample[i],sub5$date[i],sub5$time[i]))
    
     }
    })
    subdat <- droplevels(subdat)
    return(subdat)
    })
  })
  output$ivtable <- renderTable({
    temp <- group_by(subdat1(),sample,date,time)
    ivtable <- summary(temp)
    return(ivtable)
  })
  
  #output for UI on page "plot IV"
  output$ui_sample <- renderUI({
    selectInput("selectsample", label = h3("Select Sample"),choices = levels(subdat1()$sample))
  })
  output$ui_date <- renderUI({
    if(is.null(input$selectsample)){
      selectInput("selectdate", label = h3("Select Date"),choices = levels(subdat1()$date))
      }
    else{
      temp <- subset(subdat1(),sample == input$selectsample)
    selectInput("selectdate", label = h3("Select Date"),choices = levels(temp$date))
    }
  })
  IVtimes <- reactive({
    if(is.null(input$selectsample)|is.null(input$selectdate)){}
    else{
      #browser()
      temp <- subset(subdat1(),sample == input$selectsample)
      temp <- subset(temp,date == input$selectdate)
      temp <- droplevels(temp)
      temp <- data.frame(unique(temp$time))
      return (temp)}
  })
  output$IVtimes<- renderTable(
    IVtimes()
  )
  output$ui_time <- renderUI({
    
    sliderInput("selecttime", label = h3("Select Time"), min = 1, max = dim(IVtimes())[1],value = c(1,dim(IVtimes())[1]))
    
    })
  
  #iv data subset for plot
  subdat2 <- reactive({
    temp <- filter(subdat1(),(sample == input$selectsample)&(date == input$selectdate)&(time %in% IVtimes()[input$selecttime[1]:input$selecttime[2],1]))
    return (temp)
      })
  #plot iv curve
  output$ivplot <- renderPlot({
    see(subdat2())
  })
  
  #data summaries
  curves.m <- reactive({
    curves <- group_by(subdat1(),date,sample)
    curves.m<- mark_curves(curves)
    return(curves.m)
    })
  curves.s <- reactive({
    curves.m <- group_by(curves.m(), sample, time)
    curves.s <- do(curves.m, getGoody(.))
    return(curves.s)
  })
  stat.sum1 <- reactive({
    tab <- group_by(curves.s(),time,date,sample)
    tab1 <- summarise_each_(tab, funs(unique), c("V0","pmax","type"))
    tab2 <- summarise_each_(tab, funs(max),"I0")
    tab <- inner_join(tab1,tab2)
    tab <- data.frame(tab, FF = tab$pmax/(tab$I0*tab$V0))
    return(tab)
  })
  # IU for page"stat sum"
  output$ui_sample2 <- renderUI({
    selectInput("selectsample2", label = h3("Select Sample"),choices = levels(subdat1()$sample))
  })
  output$ui_date2 <- renderUI({
    if(is.null(input$selectsample2)){
      selectInput("selectdate2", label = h3("Select Date"),choices = levels(subdat1()$date))
    }
    else{
      temp <- subset(subdat1(),sample == input$selectsample2)
      selectInput("selectdate2", label = h3("Select Date"),choices = levels(temp$date))
    }
  })
  output$barplot <- renderPlot({
   # browser()
    if(is.null(input$selectsample2)){}
    else{
    tab <- stat.sum1()
    tab <- subset(tab,sample == input$selectsample2)
    dates<- unique(tab$date)
    types<- c("few.points","small.amps","I","II","III")
    table <- matrix( NA, nrow =length(types),ncol = length(dates) )
    colnames(table) <- dates
    rownames(table) <- types
    for (i in 1:length(dates)){
       for (j in 1:length(types)){
         table[j,i] <- nrow(subset(tab,(date == as.character(dates[i]))&(type == types[j])))
       }
    }
    table <- melt(table)
    colnames(table) <- c("Type", "Date", "Counts")
    table$Type <- factor (table$Type, levels = c("I","II","III","few.points","small.amps"))
    a <- ggplot(table, aes ( Counts, x = Date, fill = Type ))+geom_bar(stat = "identity")+ scale_fill_brewer()
    return(a)}
  })
  output$stat.sum <- renderTable({
    tab <- stat.sum1()
    tab <- subset(tab, (date == input$selectdate2)&(sample == input$selectsample2))
    return(tab)
  })
  
  #UI for page "identify change points"
  output$ui_sample3 <- renderUI({
    selectInput("selectsample3", label = h3("Select Sample"),choices = levels(subdat1()$sample))
  })
  output$ui_date3 <- renderUI({
    if(is.null(input$selectsample3)){
      selectInput("selectdate3", label = h3("Select Date"),choices = levels(subdat1()$date))
    }
    else{
      temp <- subset(subdat1(),sample == input$selectsample3)
      selectInput("selectdate3", label = h3("Select Date"),choices = levels(temp$date))
    }
  })
  output$ui_type3 <- renderUI({
    if((is.null(input$selectsample3))|(is.null(input$selectdate3))){
      selectInput("selecttype3", label = h3("Select Type"),choices = levels(as.factor(curves.s()$type)))
    }
    else{
      temp <- subset(curves.s(),(sample == input$selectsample3)&(date == input$selectdate3))
      selectInput("selecttype3", label = h3("Select Type"),choices = levels(as.factor(temp$type)))
    }
  })
  output$ui_time3 <- renderUI({
    if((is.null(input$selectsample3))|(is.null(input$selectdate3))|(is.null(input$selecttype3))){}
    else{
      temp <- subset(curves.s(),(sample == input$selectsample3)&(date == input$selectdate3)&(type == input$selecttype3))
      temp <- droplevels(temp)
      selectInput("selecttime3", label = h3("Select Time"),choices = levels(as.factor(temp$time)))
    }
  })
  #identify change point plot
 
  output$cpplot <- renderPlot({
    input$plotivcp
    isolate({
      #browser()
      curves.m <- curves.m()
      curves.s <- curves.s()
     a <-seeIV(mydate = input$selectdate3, mytime = input$selecttime3, mysample = input$selectsample3, dat.m = curves.m, dat.s = curves.s)
    }) 
    return(a)
     })
})


