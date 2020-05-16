#Code tried for implementing real time

fnSrv_realTimeData <- function(input,output,session,DT_stats){
    #browser()
    print("loading realtime")
    what_metrics <- yahooQF(c("Symbol"
                              ,"Last Trade (Price Only)"
                              ,"Open"
                              # ,"Days High"
                              # ,"Days Low"
                              # ,"Volume"
    ))

    #unique(DT_stats$symbol)
    temp <- getQuote('A2M.AX', what=what_metrics,row.names = F) %>% setDT()
    setnames(temp,"Symbol","symbol")
    temp <- merge(temp,DT_stats,all.x = T, by = "symbol")

    if(exists("rv") == F){
        rv <- reactiveValues(DT_realTime=NULL)
        rv$DT_realTime <- temp
        assign("rv",rv,envir = .GlobalEnv)
    } else {
        rv$DT_realTime <- rbind(rv$DT_realTime,temp)
    }

    #browser()
    print(rv$DT_realTime)
    #print(input$lovDeep_stock)
    # output$plotDeep_Daily <- renderPlot({ fn_plotDaily(rv$DT_realTime,input$lovDeep_stock,input$dt_range[1],input$dt_range[2]) },height = 590)
    #saveRDS(DT_realTime,file=here::here("100_data_raw-input",paste("DT_realTime_",strftime(Sys.Date(),format = "%d%b%y"),".Rds",sep="")))
    print("completed realtime")

}

fnSrv_realTimeData <- function(input,output,session,DT_stats){
    
    print("loading realtime")
    what_metrics <- yahooQF(c("Symbol"
                              ,"Last Trade (Price Only)"
                              ,"Open"
                              # ,"Days High"
                              # ,"Days Low"
                              # ,"Volume"
    ))

    temp <- getQuote(unique(DT_stats$symbol), what=what_metrics,row.names = F) %>% setDT()
    setnames(temp,"Symbol","symbol")
    #temp <- merge(temp,DT_stats,all.x = T, by = "symbol")

    temp <- reactive({  temp <- merge(temp,DT_stats,all.x = T, by = "symbol")
    temp
    })


    if(exists("DT_realTime()") == T){
        DT_realTime <- reactive({rbind(DT_realTime(),temp())})
        #assign("DT_realTime()",DT_realTime(),envir = .GlobalEnv)

    }else{
        DT_realTime <- temp
        #assign("DT_realTime()",DT_realTime(),envir = .GlobalEnv)
    }

    print(DT_realTime)
    print(DT_realTime())
    output$plotDeep_Daily <- renderPlot({ fn_plotDaily(DT_realTime(),input$lovDeep_stock,input$dt_range[1],input$dt_range[2]) },height = 590)
    #saveRDS(DT_realTime,file=here::here("100_data_raw-input",paste("DT_realTime_",strftime(Sys.Date(),format = "%d%b%y"),".Rds",sep="")))
    print("completed realtime")

}