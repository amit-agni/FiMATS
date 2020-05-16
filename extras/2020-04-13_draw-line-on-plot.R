library(shiny,tidyverse)

ui <- fluidPage(
    plotOutput("plot", width = "500px", height = "500px",
               hover=hoverOpts(id = "hover", delay = 1000, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
               click="click"))


server <- function(input, output, session) {
    
    plot <- ggplot(mtcars,aes(x=wt,y=qsec)) + geom_point()
    
    observe({
        if(is.null(input$click$x)){
            output$plot <- renderPlot({plot})
            }else{
                print(input$click$x)
                print(input$click$y)
                
                print(input$hover$x)
                print(input$hover$y)
                
               plot + geom_segment(aes(x=isolate(input$click$x)
                                        ,y=isolate(input$click$y)
                                        ,xend=isolate(input$hover$x)
                                        ,yend=isolate(input$hover$y)))
            }
        })
    
    
    
    
    observeEvent(input$click,{
    
    
    
    })
        
}


shinyApp(ui, server)