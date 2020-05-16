#UI and Server function definitions for the My Investments page

###############     Called from ui.R      ################

fnUI_myInvestments <- function(){
    fluidPage(
        fluidRow(column(width = 12,uiOutput("valueBoxes_myShares"))
                 ,fluidRow(
                     column(width = 12,uiOutput("box_mySharesPlot")
                            #Disable flickering during Refresh
                            ,tags$style(type="text/css", "#box_mySharesPlot.recalculating { opacity: 1.0; }")
                     )
                 )
        )
    )
}

###############     Called from server.R      ################
fnServer_myInvestments <- function(input,output,session){
    
    observe({
        req(DT_hist())
        req(DT_stats())
        req(DT_realTime())
        
        #Value boxes
        output$valueBoxes_myShares <- renderUI({validate(need(DT_myShares()$symbol,"Data is not loaded, please load data using the Data Processing Page"))
            
            if (input$radio_realTimeYN == "Yes") {
                temp_vb_myShares <-
                    merge(DT_realTime()[, .SD[which.max(`Trade Time`)], symbol]
                          ,DT_hist()[, .SD[which.max(date)], symbol][, .(symbol, close)]
                          ,all.x = T,by = "symbol")[, .(symbol, category, Last, name, growth = (Last / close) -
                                                            1)][category == 'stock'][symbol %in% unique(DT_myShares()$symbol)]
                
                setnames(temp_vb_myShares, "Last", "value")
            } else{
                temp_vb_myShares <-
                    DT_hist()[category == "stock" &
                                  !is.na(close), lapply(.SD[order(-date)], function(x)
                                      head(x, 2)), symbol][, .(date,name,close,
                                                               prev_close = shift(close, n = 1L, type = "lead")
                                                               ,growth = (close / shift(close, n = 1L, type = "lead")) - 1)
                                                           , symbol][!is.na(prev_close)][symbol %in% unique(DT_myShares()$symbol)]
                setnames(temp_vb_myShares, "close", "value")
                
            }
            lapply(1:nrow(temp_vb_myShares), function(i) {
                icon <-if (temp_vb_myShares[i]$growth < 0) {icon("thumbs-down")} else{icon("thumbs-up")}
                color <-if (temp_vb_myShares[i]$growth < 0) {"red"} else{"green"}
                valueBox(value = tags$p(format(round(temp_vb_myShares[i]$value, 1), big.mark = ","), style = "font-size:60%;")
                         ,subtitle = tags$p(paste(temp_vb_myShares[i]$name," ",round(100 * temp_vb_myShares[i]$growth, 2),"%",sep = ""),style = "font-size:80%;")
                         ,icon = NULL,color = color,width = 1)
            })
        })
        
        
        #Dynamic height plot inside a box
        var_plot <- reactive({
            if (input$radio_realTimeYN == "Yes") {
                fn_plotRealTime(DT_realTime = DT_realTime()
                                #,DT_stats=DT_stats()
                                ,varSymbols = NULL
                                ,DT_myShares = DT_myShares())
            } else{
                fn_plotYTD(DT_hist = DT_hist()
                           ,dt_start = input$dt_start
                           ,dt_end = input$dt_end
                           ,varSymbols = NULL
                           ,DT_myShares = DT_myShares())
            }
            
        })
        
        var_dynamicHeight <- reactive(gg_facet_nrow(var_plot()))
        
        output$box_mySharesPlot <- renderUI({
            req(DT_myShares()$symbol)
            
            box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "My Shares"
                ,renderPlot({var_plot()}, height = function() {var_dynamicHeight() * FACET_ROW_HEIGHT})
            )
            
        })
        
        
    }) #end observe my shares
    
    
    
    
    
}