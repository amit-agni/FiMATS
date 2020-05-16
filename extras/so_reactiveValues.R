library(shiny)

ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            actionButton("update", "Update")
        ),
        
        mainPanel(
            column(6, tableOutput('mytable')),
            column(6, tableOutput('mytable2'))
        )
    )
)

server <- function(input, output) {
    
    values <- reactiveValues(df   = RenderMyTable())
    
    # observeEvent(invalidateLater(1000), {
    #     values$df <- RenderMyTable()  # This does not update after 1 sec
    # })
    
    
    observe( {
        invalidateLater(10000)
        values$df <- RenderMyTable()  # This updates after 1 sec WORKS
    })
    
    
    observeEvent(input$update, {
        values$df <- RenderMyTable()  # This does update upon clicking
    })
    
    output$mytable  <- renderTable(values$df)  # Depends on reactiveValues
    
    autoInvalidate <- reactiveTimer(10000)
    
    output$mytable2 <- renderTable({
        autoInvalidate()
        RenderMyTable()  # >This does update after 1 sec
    })
}

time1 <- Sys.time()  # Start time
df <- data.frame(a = 1:1000)  # Some data

RenderMyTable <- function(){
    # Seconds since start time
    time2 <- as.integer(difftime(Sys.time(), time1, units="secs"))
    
    df.now <- df[1:time2,]  # Updates each second
    
    df.now
}

shinyApp(ui = ui, server = server)
