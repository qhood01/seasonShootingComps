source("helpers.R")

shinyServer(function(input, output) {

    output$years1 <- renderUI({
        years1 <- shooting[[input$p1]][["year"]]
        selectizeInput("y1",
                       label="Choose a Year",
                       choices=years1,
                       selected=NULL)
    })
    output$years2 <- renderUI({
        years2 <- shooting[[input$p2]][["year"]]
        selectizeInput("y2",
                       label="Choose a Year",
                       choices=years2,
                       selected=NULL)
    })
    output$graph1 <- renderPlot({
        compare_stats(input$p1,input$y1,input$p2,input$y2)
    })

})
