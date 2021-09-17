# Additional Info Module

infoModuleUI <- function(id) {
  
  # Create namespace using the id
  ns <- NS(id)
  
  # UI portion
  mainPanel(h1("Purpose"),
            htmlOutput(ns("purpose")),
            h1("Assumptions"),
            htmlOutput(ns("assumptions")),
            h1("Clarifications"),
            htmlOutput(ns("clarifications")),
            h1("Contact"),
            htmlOutput(ns("contact")),
  )
  
} # End of UI function


infoModuleServer <- function(input, output, session) {
  
  # Get namespace from the session
  ns <- session$ns
  
  # Display Text
  
  output$purpose <- renderText({
    "Trying to plan for retirement can be difficult.  After researching personal finance for years, I've created this interactive dashboard to help myself run simulations to appreciate the time value of money and help myself get into good investing habits by looking at six of the most important personal finance factors. I wanted to share this with others so that they can explore their finances as well to hopefully help them along their personal finance journies.
        <br><b>***NOTE: THIS IS NOT FINANCIAL ADVICE!***</b>"
  })
  
  output$assumptions <- renderText({
    "1. Inflation is set at 3% annually.<br>
        2. ROI in the stock market is set at 6% annually.<br>
        3. Social secuirty is not taken into account.<br>
        4. Tax burdens are not taken into account when money is withdrawn"
  })
  
  output$clarifications <- renderText({
    "1. The first point reflects your current snapshot for January 1 current year.<br>
        2. All points reflect snapshots and ages at January 1 of the year mentioned.<br>"
  })
  
  output$contact <- renderText({
    "For any inquires, please contact michaelwfouts@gmail.com."
  })
  
} # End of server function