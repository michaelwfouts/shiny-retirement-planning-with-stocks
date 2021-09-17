shinyServer(function(input, output) {
    
    # This is using the suggested module format for Shiny 1.5.0 and after
    calculatorModuleServer("calculatorModule")
    
    # This is using the suggested module format for before Shiny 1.5.0
    callModule(module = infoModuleServer, id = "infoModule")

})