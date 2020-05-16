library(shiny)
ui <- fluidPage(
    h4("Click on plot to start drawing, click again to pause"),
    sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
    actionButton("reset", "reset"),
    plotOutput("plot", width = "500px", height = "500px",
               hover=hoverOpts(id = "hover", delay = 10000, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
               click="click"))


server <- function(input, output, session) {
    vals = reactiveValues(x=NULL, y=NULL)
    draw = reactiveVal(FALSE)
    
    observeEvent(input$click, handlerExpr = {
        temp <- draw(); draw(!temp)
        if(!draw()) {
            vals$x <- c(vals$x, NA)
            vals$y <- c(vals$y, NA)
        }
        
        
            # print(vals$x[1])
            # print(vals$x[length(vals$x)-1])
            # 
            # print(vals$y[1])
            # print(vals$y[length(vals$y)-1])
      
            
            
        })
    
    observeEvent(input$reset, handlerExpr = {
        vals$x <- NULL; vals$y <- NULL
    })
    
    # observeEvent(input$hover, {
    #     if (draw()) {
    #         vals$x <- c(vals$x, input$hover$x)
    #         vals$y <- c(vals$y, input$hover$y)
    #     }})
    # 
    
    output$plot= renderPlot({
        # req(vals$x)
        # req(vals$y)
        # browser()
        
        print(input$click$x)
        print(input$click$y)
        
        print(input$hover$x)
        print(input$hover$y)
        
        if(is.null(input$click$x) | is.null(input$hover$x)){
            x <- 0
            y <- 0
            xend <- 0
            yend <- 0
            ggplot(mtcars,aes(x=wt,y=qsec)) + geom_point()
        }else{
            x=vals$x[1]
            y=vals$y[1]
            xend=vals$x[length(vals$x)-1]
            yend=vals$y[length(vals$y)-1]
            ggplot(mtcars,aes(x=wt,y=qsec)) + geom_point() +
                #geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) +
                #geom_segment(aes(x=x,y=y,xend=input$hover$x,yend=input$hover$y)) +
                #geom_segment(aes(x=input$click$x,y=input$click$y,xend=input$hover$x,yend=input$hover$y)) +
                geom_point(aes(input$hover$x,input$hover$y)) 
                #geom_point(aes(x=xend,y=yend))
            
        }

        
#        plot(x=vals$x, y=vals$y, xlim=c(0, 28), ylim=c(0, 28), ylab="y", xlab="x", type="l", lwd=input$mywidth)

    })

    
    }
shinyApp(ui, server)