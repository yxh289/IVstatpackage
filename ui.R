#Yang Hu
#V0.00.04
#V0.00.03

library(shiny)
shinyUI(navbarPage("I-V curve stats package",
                   
                   ##Page data input 
                   ################
                   
                   tabPanel("Data Input",
          
                            sidebarLayout(
                              sidebarPanel (
                                #select work directory
                                textInput("workdir1",label = h3("Enter your data files directory")),
                                #p("Current work directory is "),
                                #fluidRow(column(6, verbatimTextOutput("wd"))),
                                checkboxInput("workdir", label = ("Use example data files directory"), value = TRUE),
                                p("Current data directory is set as :"),            
                                fluidRow(column(10, verbatimTextOutput("textinput"))),
                                actionButton("wdset","Set data directory"),
                                #select sample
                                uiOutput("ui_sample0"),
                                hr(),
                                #fluidRow(column(6, verbatimTextOutput("value1"))),
                                #select date range
                                dateRangeInput("dates", label = h3("Date range"),start = "2013-03-20",end = "2014-11-04"),
                                hr(),
                                #fluidRow(column(6, verbatimTextOutput("value2"))),
                                #sample iv curves in a day or not
                                checkboxInput("sampleiv", label = h4("Randomly subsample I-V curves in each day"), value = FALSE),
                                
                                hr(),
                                #fluidRow(column(6, verbatimTextOutput("value3"))),
                                #sample size
                                numericInput("samplenum", label = h4("Random sample size"), value = 10),
                                
                                hr(),
                                fluidRow(column(6, verbatimTextOutput("value4"))),
                                #inport data
                                actionButton("import","Import Data"),
                                p("Click on the Import Data button to load selected dataset for further analysis.")  
                                
                              ),
                              
                              mainPanel(plotOutput("histPlot"))
                            )
                            
                                  
                   ),
                   ##Page data subset
                   #################
                   
                   tabPanel("Data subset",
                            verbatimTextOutput("nText"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("ui_num"),
                                hr(),
                                p("Current Value:", style = "color:#888888;"), 
                                verbatimTextOutput("slider_num"),
                                
                                uiOutput("ui_temp"),
                                hr(),
                                p("Current Value:", style = "color:#888888;"), 
                                verbatimTextOutput("slider_temp"),
                                checkboxInput("extremetemp","Eliminate extreme temperature records", value = TRUE),
                                checkboxInput("NAtemp","Eliminate NA temperature records",value = TRUE),
                                
                                uiOutput("ui_irr"),
                                hr(),
                                p("Current Value:", style = "color:#888888;"), 
                                verbatimTextOutput("slider_irr"),
                                checkboxInput("extremeirr","Eliminate extreme irradiance records",value = TRUE),
                                
                                uiOutput("ui_irrdiff"),
                                hr(),
                                p("Current Value:", style = "color:#888888;"), 
                                verbatimTextOutput("slider_irrdiff"),
                                #subset data
                                actionButton("subset1","Subset Data"),
                                p("Click on Subset Data botton to trim the data set according to the range selected above")
                              ),
                              mainPanel (
                                plotOutput("sumboxplot"),
                                br(),
                                tableOutput("sumtable1")
                               
                              )
                            )
                            
                   ),
                   tabPanel("I-V curve Visualization",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("ui_sample"),
                                uiOutput("ui_date"),
                                fluidRow(column(6, tableOutput("IVtimes"))),
                                uiOutput("ui_time"),
                                actionButton("plotiv", label = "Plot I-V Curve")
                              ),
                              mainPanel (
                                plotOutput("ivplot"),
                                hr(),
                                tableOutput("ivtable")
                                         
                                         ) 
                             )
                            ),
                   tabPanel("Statistics Summary",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("ui_sample2"),
                                uiOutput("ui_date2")
                              ),
                            mainPanel(
                              plotOutput("barplot"),
                              tableOutput("stat.sum")
                            )  
                            )
                            ),
                   tabPanel("I-V curve change points identify",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("ui_sample3"),
                                uiOutput("ui_date3"),
                                uiOutput("ui_type3"),
                                uiOutput("ui_time3"),
                                actionButton("plotivcp",label ="Identify Change Points"),
                                p("Click to update tge plot on right")
                              ),
                              mainPanel(
                                plotOutput("cpplot")
                                
                              )  
                            )
                            ),
                   tabPanel("About",
                            navlistPanel(
                            tabPanel("Introduction",
                                     p("The I-V curve contains useful electrical data on solar cell or commercial module performance. Typically, to analyze these shapes researchers turn to physics-based models. 
                                        The single diode model is one physical model that explains the I-V well, but implicit in any model are the assumptions of validity. For example, bypassed substrings are typically removed from study because they do not fit a model easily. 
                                        As we show below in the figure, non-ideal curves (with bypass diodes on) are useful and an element of the real-world that should not be excluded."),
                                     img(src = "1502-NonIrradIV-1CellRsHigh.png",height = 400),
                                     p("Figure 1. A simulation of a 60 cells PV modules using a numerical engine and circuit solver tool shows that, it is not possible to resolve the differences in I-V, Pmp datastreams where the Rs is equivalent but distributed equally over 60 cells, or concentrated in 1 cell, unless the irradiance is nonuniform as in soiling, or snow cover. According to these results we should find a method of finding the bypass diode turn-on voltage very precisely.
"),
                                     p("From Solar Durability and Lifetime Extension (SDLE) center’s outdoor PV module test facility -- SDLE SunFarm, we have acquired over 1 million I-V, Pmp datastreams over 500 days. Hence automated algorithmic analytics are a necessity. 
                                       The real-world is plagued by non-idealities, but according to the simulation shown above, we can extract information from these data, such as the variance in cell behavior.
                                       Indeed, simply counting the number of bypassed curves over time is useful in determining if substrings have deteriorated away from the norm. 
                                       Non-uniform irradiance is caused by e.g. snow cover, soiling, shading, etc."),
                                     p("The I-V curve statistic package can automatically classify I-V, Pmp datastreams by the number of change points, or bypassed strings. Type I curves show 0 change points, Type II show 1, and Type III show 2. For the first time, to our knowledge, we can automatically process preliminary diode model parameters on non-uniformly illuminated I-V, Pmp datastreams and extract the shunt resistance of each substring and bypass diode turn on voltage. The method was developed by simply fitting an I-V, Pmp datastream to a moving local regression model and seeking points where the local error function spikes - the change point is identified and captured."
                                     )),
                            #tabPanel("Type II",img(src="uitype1.png", height = 600, width = 600)),
                            #tabPanel("Type II",img(src="uitype2.png", height = 600, width = 600)),
                            #tabPanel("Type III",img(src="uitype3.png", height = 600, width = 600)),
                            #tabPanel("Type Low Amp",img(src="uilowamp.png", height = 600, width = 600)),
                            tabPanel("Authors",
                                     h3("Yang Hu"),
                                     h4("Candidate of Doctor of Philosopy, Case Western Reserve University"),
                                     h5("Solar Durability and Lifetime Extension (SDLE) center",a("Link", href="http://engineering.case.edu/centers/sdle/sdle/node/84")),
                                     p("Ph.D. research \"Extract Degradation of Electrical Parameters of PV Modules from Current-Voltage Characteristics\" focus on quantitatively evaluate PV modules performance by extracting electrical response parameters from real-world and lab-based current-voltage curve measurements."))
                  
                            )
                            
                            )
))