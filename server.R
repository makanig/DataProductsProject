## Developing Data Products - course project
##
library(UsingR)
library(shiny)
library(ggplot2)
library(reshape2)

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 


shinyServer(
  function(input, output) {
    
    hcosts <- reactive({
      getForecast(input$numYears)
    })
    
    output$healthCostPlot <- renderPlot({
      
      
      df <- hcosts()
      dat <- getDat()
      
      df_long1 <- melt(dat,id="Year")
      df_long2 <- melt(df, id="Year")  # convert to long format
            
      p <- ggplot(data= dat, aes(x=Year, y = value, color = Legend)) + 
        geom_line(aes(y = Health, col = "Health"),  size=1) + 
        geom_line(aes(y = GDP, col = "GDP"), size=1) +
        geom_smooth(aes(x=Year, y=Health, ymax=HealthHi, ymin=HealthLo), 
                    colour='blue', size=1, data=df, stat='identity') +
        geom_smooth(aes(x=Year, y=GDP, ymax=GDPHi, ymin=GDPLo), 
                    colour='red', size=1, data=df, stat='identity') +
        labs(title = "Forecast of Healthcare costs compared to GDP") +
        labs(x="Year") +
        labs(y="2009 Dollars (Billions)")

      print(p)
      
    }, height=600)
    
    # Show the first "n" observations
    output$summary <- renderTable({
      df <- hcosts()
      df2 <- subset(df, select=c("Year", "GDP", "Health"))
      head(df2)
    })
    
   
    
  }
)

setdir <- function() {
  #setwd("C:/Users/gautam.Mom81/Documents/courseraDataScienceSpecialization/developingDataProducts/project")
}


## Implement caching
getDat <- function() {
  
  if (exists("datRetOuter")) {
    datRet <- datRetOuter
    return(datRet)
  }
  
  load("openxlsx")
  load("reshape2")
  library(openxlsx)
  library(forecast)
  setdir()
  
  print(getwd())
  #setwd("data")
  
  dfile <- "data/MEPS.xlsx"
  dat <-  openxlsx::read.xlsx(dfile, startRow=2, sheet=3)
  
  # add the GDP data
  g <- openxlsx::read.xlsx(dfile, sheet = 4, rows=17:20)
  g <- g[1,]
  colnames(g) <- colnames(dat)
  
  dat <- rbind(dat,g)
  categories <- dat[[1]]
  
  dat <-  dat[,-1]
  dat <- t(dat)  # transpose the data frame and remove the first column
  colnames(dat) <- categories
  
  dat <- data.frame(dat)
  dat$Year <- as.numeric(rownames(dat))
  colnames(dat)[36] <- "GDP"
  datRet <- subset(dat, select=c("Year", "GDP", "Health"))
  datRetOuter <<-datRet
  datRet
}

getForecast <- function(numYears=40) {
  
  dat <- getDat()

  tsHlth <-  ts(dat$Health, start = 2000)
  #fitHlth <- ets(tsHlth, model="ZZN")
  
  tsGdp <-  ts(dat$GDP, start = 2000)
  #fitGdp <- ets(tsGdp, model="ZZN")
  
  #plot(fit$fitted.values, fitBATS$y)
  
  # get the next numYears forecasts
  fcastHlth <- data.frame(forecast(tsHlth,  h = numYears, level=95, allow.multiplicative.trend=TRUE))
  fcastGdp <- data.frame(forecast(tsGdp,  h = numYears, level=95, allow.multiplicative.trend=TRUE))
  
  f <- data.frame(Year = as.numeric(rownames(fcastHlth)),
                 Health = fcastHlth$Point.Forecast,
                 HealthLo = fcastHlth$Lo.95,
                 HealthHi = fcastHlth$Hi.95,
                 GDP = fcastGdp$Point.Forecast,
                 GDPLo = fcastGdp$Lo.95,
                 GDPHi = fcastGdp$Hi.95
                 )
  
}


  
 
  