#UI and Server function definitions for the Data Loading page

###############     Called from ui.R      ################
fnUI_dataRefresh <- function(){
    
    verticalLayout(
        box(collapsible = T,solidHeader = T,status = "primary",title ="Data Storage"
            ,radioButtons('radio_newSavedData',label="Data Load"
                          ,choiceValues = list("new","saved")
                          ,choiceNames = list("Load fresh data using symbols in CSV file"
                                              ,"Load previously saved data and update it if needed")
                          ,selected = "saved"
                          )
            ,conditionalPanel(condition = "input.radio_newSavedData == 'new'"
                              ,dateInput("date_dataStartDate","Start date for loading the data",format = "dd-M-yy")
                              ,fileInput("file_yahooCodes", "CSV File with symbols as in Yahoo"
                                         ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                         ,width = '100%')
                                         ,"Note : The file should be in CSV format and must have these columns : country, category, sector, symbol, name. 
                              All the columns except name are mandatory"
                              ,br(),br()
                              ,radioButtons('radio_saveDataYN',label="Save data for future use (needs Dropbox)"
                                            ,choices = c("Yes","No"),selected = "No",inline = T)
                              ,actionButton("ab_loadFreshData","Load"))
            ,conditionalPanel(condition = "input.radio_newSavedData == 'saved'"
                              ,actionButton("ab_loadSavedData","Load")
                              ,br(),br()
                              ,textOutput('txt_histDataStatus')
                              ,br(),br()
                              )
            )
        ,box(collapsible = T,solidHeader = T,status = "primary",title ="Data for My Shares"
             ,radioButtons('radio_newSavedMyShares',label="Data Load"
                           ,choiceValues = list("new","saved")
                           ,choiceNames = list("Load new data from CSV file"
                                               ,"Load previously saved data")
                           ,selected = "saved"
                           )
             ,conditionalPanel(condition = "input.radio_newSavedMyShares == 'new'"
                               ,fileInput("file_myShares", "CSV File with my Shares"
                                          ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                          ,width = '100%')
                                          ,"Note : The file should be in CSV format and must have these columns : symbol, transaction_date, transaction_type, price, qty
                               All the columns except qty are mandatory"
                               ,br(),br()
                               ,radioButtons('radio_saveDataYNMyShares',label="Save data for future use (needs Dropbox)"
                                             ,choices = c("Yes","No"),selected = "No",inline = T)
                               ,actionButton("ab_loadMySharesNew","Load")
                               )
             ,conditionalPanel(condition = "input.radio_newSavedMyShares == 'saved'"
                               ,actionButton("ab_loadMySharesSaved","Load")
                               ,br(),br()
                               )
             )
        )
    # ,tags$style(make_css(list('.well', 'border-width', '0px')))
    # ,tags$style(make_css(list('.well', 'background-color', 'transparent')))
}



###############     Called from server.R      ################
fnServer_dataRefresh <- function(input,output,session){
    
    
    observe({
        req(DT_hist()$date)
        output$text_LatestDate <- renderText(paste("Data updated until :",strftime(max(DT_hist()$date),format="%a %d-%b-%y")))
    })
    
    #Fresh Load : DT_hist and DT_stats based on the symbols from CSV file
    observeEvent(input$ab_loadFreshData, {
        fnHelper_shinyBusy(T, text = "Data Loading in Progress")
        
        #Get symbols from the CSV file
        infile <- input$file_yahooCodes
        DT_yahooCodes <- fread(file = infile$datapath)
        
        DT_stats_temp <- fn_getData_DTStats(DT_yahooCodes)
        DT_stats(DT_stats_temp) #assign value to the reactiveVal
        
        DT_hist_temp <- fn_getData_DThist(DT_yahooCodes, DT_stats(), input)
        DT_hist(DT_hist_temp)   #assign value to the reactiveVal
        
        if (input$radio_saveDataYN == "Yes") {
            saveRDS(DT_stats_temp,file = here::here("100_data_raw-input", "DT_stats.Rds"))
            saveRDS(cbind(DT_hist_temp, data.frame(timestamp = Sys.time()))
                    ,file = here::here("100_data_raw-input", "DT_hist.Rds")
            )
        }
        
        fnHelper_shinyBusy(F, session = session) # remove it when done
    })
    
    #Saved Load : Load data from the saved DT_hist and DT_stats
    observeEvent(input$ab_loadSavedData, {
        fnHelper_shinyBusy(T, text = "Data Loading in Progress")
        DT_stats_temp <- readRDS(file = here::here("100_data_raw-input", "DT_stats.Rds"))
        DT_hist_temp <- readRDS(file = here::here("100_data_raw-input", "DT_hist.Rds"))
        output$txt_histDataStatus <- renderText(paste0("Data loaded, it was last updated on :"
                                                       ,strftime(max(DT_hist_temp$timestamp), format = "%d%b%y %H:%M")))
        DT_stats(DT_stats_temp)
        DT_hist(DT_hist_temp)
        
        fnHelper_shinyBusy(F, session = session) # remove it when done
        
        confirmSweetAlert(session,inputId = 'alert_timeStamp',title = "Data Refresh"
                          ,text = paste0("Data was last updated on :",strftime(max(DT_hist_temp$timestamp), format = "%d%b%y %H:%M")
                                         ,"\nRefresh it ?")
                          ,type = "question",btn_labels = c("No", "Yes"),btn_colors = NULL,closeOnClickOutside = FALSE
                          ,showCloseButton = FALSE,html = FALSE)
    })
    
    
    #Updated Saved and Load : Refresh the saved DT_hist and DT_stats
    observeEvent(input$alert_timeStamp, {
        if (isTRUE(input$alert_timeStamp)) {
            fnHelper_shinyBusy(T, text = "Data Loading in Progress")
            
            DT_catchup <- tidyquant::tq_get(unique(DT_hist()$symbol),
                                            from = min(DT_hist()[, .(date = max(date)), symbol]$date - 3),
                                            to = Sys.Date(),
                                            get = "stock.prices")
            setDT(DT_catchup)
            
            #This is needed as the realtime comparison happens on prior date
            DT_catchup <- DT_catchup[date < Sys.Date()] 
            DT_catchup[DT_hist()[, .N, .(symbol, name, sector, country, category)]
                       , `:=`(name = i.name,sector = i.sector,country = i.country,category = i.category)
                       , on = "symbol"]
            DT_hist()[, timestamp := NULL]
            temp <- batchtools::ajoin(DT_catchup
                                      ,DT_hist()[!is.na(close)][, .N, .(symbol, name, sector, country, category,date)][,-"N"])
            
            DT_hist_temp <- cbind(rbind(DT_hist(), temp), data.frame(timestamp = Sys.time()))
            saveRDS(DT_hist_temp,file = here::here("100_data_raw-input", "DT_hist.Rds"))
            DT_hist(DT_hist_temp)
            output$txt_histDataStatus <- renderText(paste0("Data has been updated, latest timestamp is now :"
                                                           ,strftime(max(DT_hist_temp$timestamp), format = "%d%b%y %H:%M")))
            fnHelper_shinyBusy(F, session = session)
        }
    })
    
    
    #Load DT_myShares from CSV file
    observeEvent(input$ab_loadMySharesNew, {
        fnHelper_shinyBusy(T, text = "Data Loading in Progress")
        
        infile <- input$file_myShares
        DT_myShares_temp <- fread(file = infile$datapath)
        setDT(DT_myShares_temp)
        DT_myShares_temp[, transaction_date := lubridate::dmy(transaction_date)]
        
        if (input$radio_saveDataYNMyShares == "Yes") {
            saveRDS(DT_myShares_temp,file = here::here("100_data_raw-input", "DT_myShares.Rds"))
        }
        DT_myShares(DT_myShares_temp)
        fnHelper_shinyBusy(F, session = session)
    })
    
    #Load saved DT_myShares
    observeEvent(input$ab_loadMySharesSaved, {
        fnHelper_shinyBusy(T, text = "Data Loading in Progress")
        
        DT_myShares_temp <-readRDS(file = here::here("100_data_raw-input", "DT_myShares.Rds"))
        DT_myShares(DT_myShares_temp)
        fnHelper_shinyBusy(F, session = session)
        
    })
    
    
    #Load realtime data
    observe({
        if (input$radio_realTimeYN == "Yes") {
            invalidateLater(input$slider_realTimeDelay * 1000)
            isolate({
                DT_realTime(rbind(DT_realTime(), fn_getData_DTrealTime(DT_stats())))
            })
        }
    })
    
}

