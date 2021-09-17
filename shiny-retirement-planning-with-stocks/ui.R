# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = bs_theme(bg = "black", fg = "white", primary = "purple"),

    # Application title
    navbarPage("Retirement Planning",
               
               tabPanel("Calculator",
                        calculatorModuleUI(id = "calculatorModule")
               ), # End of Calculator Tab
               
               tabPanel("Additional Info",
                   infoModuleUI(id = "infoModule")
               ) #End of Additional Info Tab
    
    ), # End of Navbar
)) # Shiny UI and initial fluid page