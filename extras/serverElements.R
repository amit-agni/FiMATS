##################
## Data Loading ##
##################



fnSrv_loadFreshData <- function(input,output,session){
    
    #Steps
    # 1. For the codes from the CSV file
    # 2. Populate DT_stats and merge it with CSV file fields
    # 3. If name is missing in the CSV file then use the DT_stats name
    # 4. Load historical
    # 5. Merge only the CSV file fields in historical


    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    # isolate({ DT_stats })
    # isolate({ DT_hist })
    
    # DT_stats
    # DT_hist
    # 
    

        #Get symbols from the CSV file
        infile <- input$file_yahooCodes
        DT_yahooCodes <- fread(file=infile$datapath) 
        
        #For those symbols, get the key stats. what_metrics is defined in global.R
        DT_stats_temp <- getQuote(DT_yahooCodes$symbol, what=what_metrics,row.names = F)
        setDT(DT_stats_temp,keep.rownames = "symbol")
        setnames(DT_stats_temp
                 ,names(DT_stats_temp)
                 ,tolower(str_replace_all(str_replace_all(names(DT_stats_temp),"[[:punct:]]","")," ","_")))
        
        #output$txt_dataStatus <- renderText({ "Financial Stats loaded"})
        
        DT_stats_temp <- merge(DT_stats_temp,DT_yahooCodes,all.x =T,by = "symbol")
        DT_stats_temp[is.na(name),name:=namelong] #If name is not provided in the CSV file then use the yahoo name
        DT_stats_temp[,namelong:=NULL]
        
        DT_stats(DT_stats_temp)
        

    
    
        #Get symbols from the CSV file
        # infile <- input$file_yahooCodes
        # DT_yahooCodes <- fread(file=infile$datapath) 
        
        #Also, for those symbols get the Historical data
        DT_hist_temp <- tidyquant::tq_get(DT_yahooCodes$symbol,from = input$date_dataStartDate, get = "stock.prices")
        setDT(DT_hist_temp)
        DT_hist_temp <- merge(DT_hist_temp,DT_stats()[,.(symbol,name,category,sector,country)]
                         ,all.x =T,by = "symbol") #Merge only the CSV file fields in historical
        
        DT_hist_temp
        
    
        DT_hist(DT_hist_temp)    
        
    
    if(input$radio_saveDataYN == "Yes"){
        saveRDS(DT_stats,file=here::here("100_data_raw-input","DT_stats.Rds"))
        saveRDS(DT_hist,file=here::here("100_data_raw-input","DT_hist.Rds"))

    }

    
    
    output$txt_dataStatus <- renderText({ "Historical Data loaded"})
    
    
    # #Get symbols from the CSV file
    # infile <- input$file_yahooCodes
    # DT_yahooCodes <- fread(file=infile$datapath) 
    # 
    # #For those symbols, get the key stats. what_metrics is defined in global.R
    # DT_stats <- getQuote(DT_yahooCodes$symbol, what=what_metrics,row.names = F)
    # setDT(DT_stats,keep.rownames = "symbol")
    # setnames(DT_stats
    #          ,names(DT_stats)
    #          ,tolower(str_replace_all(str_replace_all(names(DT_stats),"[[:punct:]]","")," ","_")))
    # output$txt_dataStatus <- renderText({ "Financial Stats loaded"})
    # 
    # DT_stats <- merge(DT_stats,DT_yahooCodes,all.x =T,by = "symbol")
    # DT_stats[is.na(name),name:=namelong] #If name is not provided in the CSV file then use the yahoo name
    # DT_stats[,namelong:=NULL]
    
    # #Also, for those symbols get the Historical data
    # DT_hist <- tidyquant::tq_get(DT_yahooCodes$symbol,from = input$date_dataStartDate, get = "stock.prices")
    # setDT(DT_hist)
    # DT_hist <- merge(DT_hist,DT_stats[,.(symbol,name,category,sector,country)]
    #                  ,all.x =T,by = "symbol") #Merge only the CSV file fields in historical
    # output$txt_dataStatus <- renderText({ "Historical Data loaded"})
    
    
    fnHelper_shinyBusy(F) # remove it when done
    
    # DT_stats <- reactive({DT_stats})
    # DT_hist <- reactive({DT_hist})

    # DT_hist <<- reactiveVal(DT_hist)
    # DT_stats <<- reactiveVal(DT_stats)
    
    #assign("DT_stats()",DT_stats(),envir = .GlobalEnv)
    #makeReactiveBinding("DT_stats",env = .GlobalEnv)
    # DT_stats <- eventReactive(input$radio_saveDataYN,{
    #     readRDS(file=here::here("100_data_raw-input","DT_stats.Rds"))
    #     })
    
}


fnSrv_loadSavedData <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    DT_stats <- reactive(readRDS(file=here::here("100_data_raw-input","DT_stats.Rds")))
    DT_hist <- reactive(readRDS(file=here::here("100_data_raw-input","DT_hist.Rds")))
    
    # fn_srvEaglePopulateLOV(input,output,session,DT_stats())
    # fn_srvDeepPopulateLOV(input,output,session,DT_stats())
    
    fnHelper_shinyBusy(F) # remove it when done
}



fnSrv_loadIncrementalData <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    DT_hist <- readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
    DT_catchup <- tidyquant::tq_get(unique(DT_hist$symbol),from =min(DT_hist[,.(date=max(date)),symbol]$date), get = "stock.prices")
    setDT(DT_catchup)
    DT_catchup[DT_hist[,.N,.(symbol,name,sector,country,category)]
               ,`:=`(name=i.name,sector=i.sector,country=i.country,category=i.category)
               ,on = "symbol"]
    temp <- batchtools::ajoin(DT_catchup,DT_hist)
    DT_hist <- rbind(DT_hist,temp)
    saveRDS(DT_hist,file=here::here("100_data_raw-input","DT_hist.Rds"))
    output$txt_dataStatus <- renderText({ "Data has been caught up"})
    
    fnHelper_shinyBusy(F)
    
    
}


fnSrv_loadMyShares <- function(input,output,session){
    
    fnHelper_shinyBusy(T,text = "Data Loading in Progress")
    
    infile <- input$file_myShares
    DT_myShares <- fread(file=infile$datapath) 
    setDT(DT_myShares)
    DT_myShares[,transaction_date := lubridate::dmy(transaction_date)]
    
    # if(input$radio_saveDataYN == "Yes"){
    #     saveRDS(DT_myShares,file=here::here("100_data_raw-input","DT_myShares.Rds"))
    # }
    
    saveRDS(DT_myShares,file=here::here("100_data_raw-input","DT_myShares.Rds"))
    
    output$txt_dataStatus <- renderText({ "My Shares Data loaded"})
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





##################
## Deep Analyis ##
##################


fn_srvDeepPopulateLOV <- function(input,output,session,DT_stats){
    
    
    output$lovDeep_all <- renderUI({
        selectInput("lovDeep_all",label = "What to analyse"
                    ,choices = list("Stocks" = DT_stats[category == 'stock']$name
                                    ,"Indices" = DT_stats[category == 'index']$name
                                    ,"Currencies" = DT_stats[category == 'currency']$name
                                    ,"Commodities" = DT_stats[category == 'commodity']$name)
                    ,selected = DT_stats[,1]$name )
    })
    
    
}




##################
##  Overall View ##
##################



# fn_srvEaglePopulateLOV <- function(input,output,session,DT_stats = NULL){
# 
#     
#     print("error here")
#     output$boxEagle_additionalParameters <- renderUI({
# 
#         box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "Additional Parameters"
#             ,radioButtons("radioEagle_selectCategory",label = "Category"
#                     ,choices = c('index'))
#             ,selectInput("lovEagle_selectCountry",label = "Country"
#                          ,choices = c('ALL'))
#             ,selectInput("lovEagle_selectSector",label = "Sector"
#                         ,choices = c('ALL'))
#             ,radioButtons("radioEagle_displayPerPage",label="Display per page",choices = c(5,10,20,"ALL"),inline = T,selected = "10"))
#     })
#     
#     observeEvent(input$radioEagle_selectCategory,{
#         
#         req(input$radioEagle_selectCategory)
#         req(input$radio_saveDataYN)
#         
#         updateSelectInput(session
#                           ,inputId = "lovEagle_selectCountry"
#                           ,choices=c('ALL',unique(DT_stats[category==input$radioEagle_selectCategory]$country)))
#         
#         updateSelectInput(session
#                           ,inputId = "lovEagle_selectSector"
#                           ,choices=c('ALL',unique(DT_stats[category==input$radioEagle_selectCategory]$sector)))
#         
#     })
#     
#     
# }



fn_srvEaglePopulateLOV <- function(input,output,session,DT_stats){

    print("errorrs here")
    output$boxEagle_additionalParameters <- renderUI({
        
        req(DT_stats())

        box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "Additional Parameters"
            ,radioButtons("radioEagle_selectCategory",label = "Category"
                          ,choices = unique(DT_stats$category)),selectInput("lovEagle_selectCountry",label = "Country"
                                                                            ,choices = c('ALL',unique(DT_stats$country)))
            ,selectInput("lovEagle_selectSector",label = "Sector"
                         ,choices = c('ALL',unique(DT_stats$sector)))
            ,radioButtons("radioEagle_displayPerPage",label="Display per page",choices = c(5,10,20,"ALL"),inline = T,selected = "10"))
    })

    observeEvent(input$radioEagle_selectCategory,{
        req(input$radioEagle_selectCategory)

        updateSelectInput(session
                          ,inputId = "lovEagle_selectCountry"
                          ,choices=c('ALL',unique(DT_stats[category==input$radioEagle_selectCategory]$country)))

        updateSelectInput(session
                          ,inputId = "lovEagle_selectSector"
                          ,choices=c('ALL',unique(DT_stats[category==input$radioEagle_selectCategory]$sector)))

    })


}
