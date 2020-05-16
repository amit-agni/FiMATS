library(shiny)

ui<- shinyUI(fluidPage(
    
    titlePanel("Incremental Plots"),      
    sidebarLayout(
        sidebarPanel(),        
        mainPanel(
            tableOutput('var')
        )
    )
))

start_time <- 100000
end_time = start_time - 1000

server<- function(input,output,session){
    
    # Initialize
    reval_omega <- reactiveVal(data.frame(st = c(start_time),et = c(end_time)))
    reval_start <- reactiveVal(start_time)
    reval_end <- reactiveVal(end_time)
    
    # update our reactiveVal 
    observe({
        invalidateLater(1000, session) # every second
        isolate({
            reval_start(reval_end()) # set start time to current end time
            reval_end(reval_start() - 1000) # set end time to start - 1000
            omega_new <- data.frame(st = reval_start(),et = reval_end()) # create a new row for the dataframe
            reval_omega(rbind(reval_omega(),omega_new)) # rbind the reval_omega() and the new row, and store result in reval_omega()
        })
    })
    output$var <- renderTable(reval_omega())
}

shinyApp(ui,server)