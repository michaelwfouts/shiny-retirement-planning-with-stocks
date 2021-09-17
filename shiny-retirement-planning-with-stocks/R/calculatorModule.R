# Calculator Module

calculatorModuleUI <- function(id) {
  
  # Create namespace using the id
  ns <- NS(id)

  # UI portion (note all inputs and outputs are ns dependent)
  sidebarLayout(
    sidebarPanel(
      autonumericInput(ns("startMoney"),
                       "Starting Money in Stocks",
                       value = 0,
                       min = 0,
                       currencySymbolPlacement = "p",
                       currencySymbol = "$",
                       align = "left",
                       emptyInputBehavior = 0),
      autonumericInput(ns("annualInvest"),
                       "Annual Investment Into Stocks",
                       value = 0,
                       min = 0,
                       currencySymbolPlacement = "p",
                       currencySymbol = "$",
                       align = "left",
                       emptyInputBehavior = 0),
      autonumericInput(ns("currentSpending"),
                       "Current Spending in Today's dollars",
                       value = 0,
                       min = 0,
                       currencySymbolPlacement = "p",
                       currencySymbol = "$",
                       align = "left",
                       emptyInputBehavior = 0),
      numericInput(ns("currentAge"),
                   "Curent Age",
                   value = 18,
                   min = 18),
      numericInput(ns("retireAge"),
                   "Age at Retirement",
                   value = 50,
                   min = 19),
      numericInput(ns("deathAge"),
                   "Age at Death",
                   value = 95,
                   min = 20)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(ns("plot1"))
    )
  )
} # End of UI function


calculatorModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Begin code for calculator
      funRetireSimulation <- function(){
        ####Prevent Age Input Crashing####
        
        # If currentAge <= retireAge <= deathAge, it causes the application to crash
        # Current age can't be less than 18
        currentAge <- if(input$currentAge <= 18 | is.na(input$currentAge)){
          18
        } else {
          input$currentAge
        }
        
        # Retire age can't be less than current age
        retireAge <- if(input$retireAge <= currentAge | is.na(input$retireAge)){
          currentAge + 1
        } else {
          input$retireAge
        }
        
        # Death age can't be less than retire age
        deathAge <- if(input$deathAge <= retireAge | is.na(input$deathAge)){
          retireAge + 1
        } else {
          input$deathAge
        }
        
        ####Application Logic####
        #Start a Data Frame with a year column, but this will include all of the long term financial info
        current_year <- as.integer(format(Sys.Date(), "%Y"))
        
        # Look at time frame current year (assume Jan 1) to Jan 1 of year you plan on living to
        df <- data.frame(
          "Year" = c(current_year:(current_year + (deathAge - currentAge)))
        )
        
        #Next, create a calculated column of projected inflation from 2021 dollars
        inflation <- 1.03 #3% inflation is high, but probably good for this analysis
        
        df <- mutate(df, "Inflation_Rate" = inflation**(Year-current_year))
        
        #How much in Yearly Spending will I have each year accounting for inflation?
        df <- mutate(df, "Yearly_Spending" = input$currentSpending * Inflation_Rate)
        
        #How much will I have in assets do I have each year? Assume 60% of current assets in stock market and only those gain value.  Calculate through end of analysis and we'll worry about withdrawing later on by correcting the equation with an if else.
        Invest_ROI <- 1.06
        Yearly_Stock_Savings <- input$annualInvest
        
        #Since this doesn't like doing recursion in a single line, I'm writing a for loop to do the analysis
        df <- mutate(df, "Stock_Assets" = input$startMoney)
        
        #I really didn't want to hard code numbers, but I had to in order for this to work
        for (index in c(2:(deathAge - currentAge))) {
          df[index,4] <- (df[index - 1,4] + Yearly_Stock_Savings)*Invest_ROI
        }
        
        #So, looking at retirement, I would retire at age 55 which is 2050.  Since I would start taking money out then, the equation becomes slightly different and I need to factor in me drawing from it then.
        
        p <- plot_ly(type = "scatter",
                     mode = "marker") %>%
          
          layout(title = "Retirement Age Simulation",
                 yaxis = list(title = "Stock Assets"),
                 xaxis = list(title = "Year"),
                 legend=list(title=list(text='<b>Retirement Age</b>')),
                 paper_bgcolor = 'transparent',
                 plot_bgcolor = 'transparent',
                 font = list(color = 'white'))
        
        # Look at ages you'd like to consider to look at retiring at
        
        # Need to create a minimum bound as this creates issues for people retiring in less than 5 years
        retireAnalysisMinBound <- if((retireAge - currentAge) <= 5){
          (retireAge - currentAge - 1)
        } else {
          5
        }
        
        for (age in c((retireAge - retireAnalysisMinBound):(retireAge + 4))){
          Retire_Age <- age
          Long_Term_Loop <- df
          
          # Recalculate retirement holdings at each interested retirement age
          for (index in c((Retire_Age-currentAge+1):(deathAge - currentAge + 1))){
            Long_Term_Loop[index,4] <- (Long_Term_Loop[index - 1,4] - Long_Term_Loop[index, 3])*Invest_ROI
          }
          
          p <- add_trace(p,
                         x = Long_Term_Loop$Year,
                         y = Long_Term_Loop$Stock_Assets,
                         name = age,
                         mode = "lines+markers")
        }
        
        output$plot1 <- renderPlotly({
          p
        })
      } # End of computational function
      
      # Create a reactive list so that I only have to have one line for the observeEvent function
      toListen <- reactive({
        list(input$startMoney,
             input$annualInvest,
             input$retireAge,
             input$currentSpending,
             input$currentAge,
             input$deathAge
        )
      })
      
      # Anytime a value changes, recalculate the graph
      observeEvent(toListen(), {
        funRetireSimulation()
      })
    }
  )
}