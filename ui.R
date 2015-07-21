shinyUI(pageWithSidebar(
  headerPanel("Forecast of escalating Healthcare Costs"),
  sidebarPanel(
    sliderInput('numYears', 'Enter number of years (past 2010) to forecast and wait for 5 seconds',
                value = 40, min = 20, max = 100, step = 1,),
    
       # adding the new div tag to the sidebar            
    tags$div(class="header", checked=NA,
             tags$p("Data source from BLS"),
             tags$a(href="http://www.bea.gov/national/health_care_satellite_account.htm", "BLS data source"),
    headerPanel("Server calculation"),    
    tableOutput("summary")
  )),
  mainPanel(
    plotOutput('healthCostPlot')
  )
  )
)