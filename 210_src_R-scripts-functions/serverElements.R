##################
## Data Loading ##
##################

fnSrv_initialLoad <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    infile <- input$file_yahooCodes
    DT_yahooCodes <- fread(file=infile$datapath) 
    
    if("Daily historical values" %in% input$chkb_loadWhat)
    {
        #Historical daily 
        DT_hist <- tidyquant::tq_get(DT_yahooCodes$symbol,from = "2005-01-01", get = "stock.prices")
        setDT(DT_hist)
        
        DT_hist <- merge(DT_hist,DT_yahooCodes,all.x =T,by = "symbol")
        saveRDS(DT_hist,file=here::here("100_data_raw-input","DT_hist.Rds"))
        output$txt_dataStatus <- renderText({ "Historical Data loaded"})
        
    }
    
    #quantmod::yahooQF()
    
    if("Financial Stats" %in% input$chkb_loadWhat)
    {
        #Financial stats
        what_metrics <- quantmod::yahooQF(c("Market Capitalization"
                                  , "Price/Sales"
                                  , "P/E Ratio"
                                  , "PEG Ratio"
                                  , "Price/EPS Estimate Next Year"
                                  , "Price/Book"
                                  , "Book Value"
                                  , "Shares Outstanding"
                                  , "Ex-Dividend Date"
                                  , "Dividend/Share"
                                  , "Dividend Yield"
                                  , "Earnings/Share"
                                  , "Price Hint"
                                  , "52-week High"
                                  , "Percent Change From 52-week High"
                                  , "52-week Low"
                                  , "Percent Change From 52-week Low"
                                  , "50-day Moving Average"
                                  , "Percent Change From 50-day Moving Average"
                                  , "200-day Moving Average"
                                  , "Percent Change From 200-day Moving Average"
                                  , "Last Trade (Price Only)"                 
                                  , "Last Trade Time"  
                                  ))
        
        DT_stats <- getQuote(DT_yahooCodes$symbol, what=what_metrics,row.names = F)
        setDT(DT_stats,keep.rownames = "symbol")
        
        DT_stats <- merge(DT_stats,DT_yahooCodes,all.x =T,by = "symbol")
        
        setnames(DT_stats
                 ,names(DT_stats)
                 ,tolower(str_replace_all(str_replace_all(names(DT_stats),"[[:punct:]]","")," ","_"))
        )
        
        
        saveRDS(DT_stats,file=here::here("100_data_raw-input","DT_stats.Rds"))
        output$txt_dataStatus <- renderText({ "Financial Stats loaded"})
    }
    
    fnHelper_shinyBusy(F) # remove it when done
    
}


fnSrv_loadMyShares <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    infile <- input$file_myShares
    DT_myShares <- fread(file=infile$datapath) 
    setDT(DT_myShares)
    DT_myShares[,transaction_date := lubridate::dmy(transaction_date)]
    saveRDS(DT_myShares,file=here::here("100_data_raw-input","DT_myShares.Rds"))
    output$txt_dataStatus <- renderText({ "My Shares Data loaded"})
    
    fnHelper_shinyBusy(F)
    
}

fnSrv_dataCatchup <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    DT_hist <- readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
    DT_catchup <- tidyquant::tq_get(unique(DT_hist$symbol),from =min(DT_hist[,.(date=max(date)),symbol]$date), get = "stock.prices")
    setDT(DT_catchup)
    DT_catchup[DT_hist[,.N,.(symbol,name,sector,country,type)]
               ,`:=`(name=i.name,sector=i.sector,country=i.country,type=i.type)
               ,on = "symbol"]
    temp <- batchtools::ajoin(DT_catchup,DT_hist)
    DT_hist <- rbind(DT_hist,temp)
    saveRDS(DT_hist,file=here::here("100_data_raw-input","DT_hist.Rds"))
    output$txt_dataStatus <- renderText({ "Data has been caught up"})
    
    fnHelper_shinyBusy(F)
    
                                   
}

fnSrv_summaryStats <- function(input,output,session){
    DATE_LOOKBACK <- Sys.Date() - 365
    
    temp <- DT_hist[,c(tail(.SD,1)[,.(close,date)]
                        ,.SD[date >= DATE_LOOKBACK][which.min(close),.(low=close,lowvolume=volume,lowdate=date
                                                                       ,lowsince=difftime(Sys.Date(),date,units="days"))]
                        ,.SD[date >= DATE_LOOKBACK][which.max(close),.(high=close,highvolume=volume,highdate=date
                                                                       ,highsince=difftime(Sys.Date(),date,units="days"))])
                     ,symbol]
    
    DT_stats <- merge(DT_stats,temp,all.x = T, by = "symbol")
    
    saveRDS(DT_stats,file=here::here("100_data_raw-input","DT_stats.Rds"))
    
    
}


fnSrv_realTimeData <- function(DT_stats){
    print("loading realtime")
    what_metrics <- yahooQF(c("Symbol"
                              ,"Last Trade (Price Only)"
                              ,"Open"
                              # ,"Days High"
                              # ,"Days Low"
                              # ,"Volume" 
    ))

    temp <- try(getQuote(unique(DT_stats$symbol), what=what_metrics,row.names = F))

    if(class(temp) != "data.frame"){
        temp <- data.frame()
        print(temp)
        print("error in real time loading")
    }else{
        setDT(temp)
        setnames(temp,"Symbol","symbol")    
    }
    print("completed realtime")
    temp
    
}



##################
## Deep Analyis ##
##################


fn_srvDeepPopulateLOV <- function(input,output,session,DT_stats){
    
    output$lovDeep_all <- renderUI({
        selectInput("lovDeep_all",label = "What to analyse"
                    ,choices = list("Stocks" = DT_stats[type == 'stock']$name
                                    ,"Indices" = DT_stats[type == 'index']$name
                                    ,"Currencies" = DT_stats[type == 'currency']$name
                                    ,"Commodities" = DT_stats[type == 'commodity']$name)
                    ,selected = DT_stats[,1]$name )
    })
    
    
}



