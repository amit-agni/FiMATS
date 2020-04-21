server <- function(input, output, session) {
  #####################
  #   Data Refresh   #
  #####################
  
  # DT_stats <- reactiveVal(data.frame())
  # DT_hist <- reactiveVal(data.frame())
  # DT_myShares <- reactiveVal(data.frame())
  
  DT_stats <-
    reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_stats.Rds")))
  DT_hist <-
    reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_hist.Rds")))
  DT_myShares <-
    reactiveVal(readRDS(file = here::here(
      "100_data_raw-input", "DT_myShares.Rds"
    )))
  
  DT_realTime <- reactiveVal(data.frame())
  
  #Load DT_hist and DT_stats based on the symbols from CSV file
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
      saveRDS(DT_stats_temp,
              file = here::here("100_data_raw-input", "DT_stats.Rds"))
      saveRDS(
        cbind(DT_hist_temp, data.frame(timestamp = Sys.time())),
        file = here::here("100_data_raw-input", "DT_hist.Rds")
      )
    }
    
    fnHelper_shinyBusy(F, session = session) # remove it when done
  })
  
  #Load saved DT_hist and DT_stats
  observeEvent(input$ab_loadSavedData, {
    fnHelper_shinyBusy(T, text = "Data Loading in Progress")
    
    DT_stats_temp <-
      readRDS(file = here::here("100_data_raw-input", "DT_stats.Rds"))
    DT_hist_temp <-
      readRDS(file = here::here("100_data_raw-input", "DT_hist.Rds"))
    
    output$txt_histDataStatus <-
      renderText(paste0(
        "Data loaded, it was last updated on :"
        ,
        strftime(max(DT_hist_temp$timestamp), format = "%d%b%y %H:%M")
      ))
    DT_stats(DT_stats_temp)
    DT_hist(DT_hist_temp)
    
    fnHelper_shinyBusy(F, session = session) # remove it when done
    
    confirmSweetAlert(
      session,
      inputId = 'alert_timeStamp',
      title = "Data Refresh"
      ,
      text = paste0(
        "Data was last updated on :"
        ,
        strftime(max(DT_hist_temp$timestamp), format = "%d%b%y %H:%M")
        ,
        "\nRefresh it ?"
      )
      ,
      type = "question",
      btn_labels = c("No", "Yes")
      ,
      btn_colors = NULL,
      closeOnClickOutside = FALSE,
      showCloseButton = FALSE,
      html = FALSE
    )
  })
  
  #Refresh the saved DT_hist and DT_stats
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
      saveRDS(DT_myShares_temp,
              file = here::here("100_data_raw-input", "DT_myShares.Rds"))
    }
    
    
    DT_myShares(DT_myShares_temp)
    
    fnHelper_shinyBusy(F, session = session)
    
  })
  
  
  #Load saved DT_myShares
  observeEvent(input$ab_loadMySharesSaved, {
    fnHelper_shinyBusy(T, text = "Data Loading in Progress")
    
    DT_myShares_temp <-
      readRDS(file = here::here("100_data_raw-input", "DT_myShares.Rds"))
    
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
  
  
  #####################
  #   Overall View    #
  #####################
  
  observe({
    output$boxEagle_additionalParameters <- renderUI({
      validate(
        need(
          DT_stats()$category,
          "Data is not loaded, please load data using the Data Processing Page"
        )
      )
      
      box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "Additional Parameters"
        ,radioButtons("radioEagle_selectCategory",label = "Category",choices = unique(DT_stats()$category))
        ,selectInput("lovEagle_selectCountry",label = "Country",choices = c('ALL', unique(DT_stats()$country)))
        ,selectInput("lovEagle_selectSector",label = "Sector",choices = c('ALL', unique(DT_stats()$sector)))
        ,radioButtons("radioEagle_displayPerPage",label = "Display per page",choices = c(8,12,24, "ALL"),inline = T,selected = "12")
      )
    })
    
    
  })
  
  observeEvent(input$radioEagle_selectCategory, {
    req(DT_stats())
    req(input$radioEagle_selectCategory)
    
    updateSelectInput(session,inputId = "lovEagle_selectCountry",
                      choices = c('ALL', unique(DT_stats()[category == input$radioEagle_selectCategory]$country)))
    updateSelectInput(session,inputId = "lovEagle_selectSector",
                      choices = c('ALL', unique(DT_stats()[category == input$radioEagle_selectCategory]$sector)))
  })
  
  observe({
    req(DT_stats())
    
    req(input$radioEagle_selectCategory)
    req(input$lovEagle_selectCountry)
    req(input$lovEagle_selectSector)
    # req(input$radioEagle_displayPerPage)
    # req(input$radio_realTimeYN)
    # req(input$dt_start)
    # req(input$dt_end)
    
    varEagle_parameters <-
      case_when(input$lovEagle_selectCountry == 'ALL' &input$lovEagle_selectSector == 'ALL' ~
          "category==input$radioEagle_selectCategory"
          ,input$lovEagle_selectCountry == 'ALL' & input$lovEagle_selectSector != 'ALL' ~
          "sector==input$lovEagle_selectSector & category==input$radioEagle_selectCategory"
          ,input$lovEagle_selectCountry != 'ALL' & input$lovEagle_selectSector == 'ALL' ~
            "country==input$lovEagle_selectCountry & category==input$radioEagle_selectCategory"
          ,input$lovEagle_selectCountry != 'ALL' & input$lovEagle_selectSector != 'ALL' ~
            "country==input$lovEagle_selectCountry & sector==input$lovEagle_selectSector & category==input$radioEagle_selectCategory"
      )
    
    #Value boxes
    output$valueBoxes_eagle <- renderUI({
      if (input$radio_realTimeYN == "Yes") {
        req(DT_realTime())
        req(DT_hist())
        temp_vb <-
          merge(DT_realTime()[eval(parse(text = varEagle_parameters)), .SD[which.max(`Trade Time`)], .(symbol, category)]
                ,DT_hist()[, .SD[which.max(date)], symbol][, .(symbol, close)] #need for growth over previous day
                ,all.x = T,by = "symbol")[
                  , .(symbol, category, Last, name, growth = (Last / close) - 1)][, head(.SD, NO_OF_VALUE_BOXES)]
        setnames(temp_vb, "Last", "value")
      } else{
        req(DT_hist())
        temp_vb <- DT_hist()[eval(parse(text = varEagle_parameters))
                             ,lapply(.SD[order(-date)], function(x)
                               head(x, 2)), symbol][
                                 , .(date,name,close,prev_close = shift(close, n = 1L, type = "lead")
                                     ,growth = (close / shift(close, n = 1L, type = "lead")) - 1), symbol][
                                       !is.na(prev_close)][, head(.SD, NO_OF_VALUE_BOXES)]
        setnames(temp_vb, "close", "value")
      }
      
      
      temp_vb <- temp_vb[!is.na(value) & !is.na(growth)]
      
      lapply(1:nrow(temp_vb), function(i) {
        icon <-if (temp_vb[i]$growth < 0) {icon("thumbs-down")} else{icon("thumbs-up")}
        color <- if (temp_vb[i]$growth < 0) {"red"} else{"green"}
        valueBox(tags$p(format(round(temp_vb[i]$value, 1), big.mark = ","), style = "font-size:60%;")
                 ,tags$p(paste(temp_vb[i]$name," ",round(100 * temp_vb[i]$growth, 2),"%",sep = "")
                         , style = "font-size:80%;")
          ,icon = NULL,color = color,width = 1
        )
      })
    })
    
    
    var_plot <- reactive({
      if (input$radio_realTimeYN == "Yes") {
        req(DT_realTime())
        fn_plotRealTime(
          DT_realTime = DT_realTime()[eval(parse(text = varEagle_parameters))]
          ,varSymbols = NULL,DT_myShares = NULL,displayPerPage = input$radioEagle_displayPerPage
        )
      } else{
        req(DT_hist())
        fn_plotYTD(
          DT_hist = DT_hist()[eval(parse(text = varEagle_parameters))]
          ,dt_start = input$dt_start,dt_end = input$dt_end,varSymbols = NULL,DT_myShares = NULL
          ,displayPerPage = input$radioEagle_displayPerPage
        )
      }
    })
    
    
    var_dynamicHeight <- reactive(gg_facet_nrow(var_plot()))
    
    output$box_plotEagle <- renderUI({
      box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = input$radioEagle_selectCategory
        ,renderPlot({var_plot()}, height = function() {var_dynamicHeight() * FACET_ROW_HEIGHT})
      )
      
    })
    
  }) #end observe
  
  
  
  
  #################
  #   My Shares   #
  #################
  
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
    
    
  }) #end observe
  
  
  
  
  ######################
  #   Deep Analysis    #
  ######################
  
  output$lovDeep_all <- renderUI({
    req(DT_stats()$category)
    selectInput(
      "lovDeep_all",
      label = "What to analyse"
      #,choices = split(DT_stats()[,.N,.(category,name)][,-"N"],by="category",keep.by=F,flatten=F)
      ,choices = list(
        "Stocks" = DT_stats()[category == 'stock']$name
        ,"Indices" = DT_stats()[category == 'index']$name
        ,"Currencies" = DT_stats()[category == 'currency']$name
        ,"Commodities" = DT_stats()[category == 'commodity']$name
      )
      ,selected = DT_stats()[1, ]$name
    )
  })
  
  
  
  
  observe({
    
    req(input$lovDeep_all)
    req(input$radioDeep_PlotYTDType)
    
    req(DT_hist())
    req(DT_stats())
    req(DT_myShares())
    req(DT_realTime())
    
    
    output$tblDeep_KPI <- renderTable({fn_tblKPI(DT_hist(), DT_stats(), input$lovDeep_all)})
    
    
    output$boxDeep_PlotYTD <- renderUI({
      #  req(DT_myShares()$symbol)
      
      box(
        collapsible = T,
        solidHeader = T,
        width = NULL,
        status = "success",
        title = "Year to date trends",
        #height = paste(PLOT_HEIGHT,"px",sep="")
        height = PLOT_HEIGHT+64
        
        ,if(input$radioDeep_PlotYTDType =='Standard'){
          renderPlotly({
            fn_plotYTD(
              DT_hist = DT_hist()
              ,dt_start = input$dt_start
              ,dt_end = input$dt_end
              ,varSymbols = input$lovDeep_all
              ,DT_myShares = DT_myShares()
              )})
        }else{
          renderPlotly({
            fn_plotFinance(
              DT_hist = DT_hist()
              ,dt_start = input$dt_start
              ,dt_end = input$dt_end
              ,varSymbols = input$lovDeep_all
              ,plotType = input$radioDeep_PlotYTDType
            )})
        }
        
        )
      
      
      
    })
    
    #renderUI(renderPlot) gives object of type 'closure' is not subsettable
    #renderUI(plotOutput) gives Text to be written must be a length-one character vector
    
    
    output$plotDeep_realTime <-
      renderPlot({
        if (input$radio_realTimeYN == "Yes") {
          fn_plotRealTime(
            DT_realTime = DT_realTime()
            ,varSymbols = input$lovDeep_all
            ,DT_myShares = NULL
          )
        }
      }
      ,height = PLOT_HEIGHT)
    
  })
  

  
  ######################
  #   Charting tools  #
  ######################
  
  output$lovCharts_all <- renderUI({
    req(DT_stats()$category)
    selectInput(
      "lovCharts_all",
      label = "What to analyse"
      #,choices = split(DT_stats()[,.N,.(category,name)][,-"N"],by="category",keep.by=F,flatten=F)
      ,choices = list(
        "Stocks" = DT_stats()[category == 'stock']$name
        ,"Indices" = DT_stats()[category == 'index']$name
        ,"Currencies" = DT_stats()[category == 'currency']$name
        ,"Commodities" = DT_stats()[category == 'commodity']$name
      )
      ,selected = DT_stats()[1, ]$name
    )
  })
  
  
  output$txt_chartingNotes <- renderText({ read_file(here::here('100_data_raw-input','charting-notes.html')) })
  
  
  observeEvent(input$lovCharts_all,{
  
    print("in chart")
    req(input$lovCharts_all)
    
    output$plotCharts_technicalChart <- renderPlot({
      
      temp <- DT_hist()[name == input$lovCharts_all & date >= input$dt_start & date <= input$dt_end]
      xts(temp[,.(open,high,low,close,volume)],order.by =temp$date)  %>%
        chartSeries(TA='addBBands();addRSI();addMACD();addVo()'
                    ,theme = "white"
                    ,name = 'Technical Charts')
      # myPars <-chart_pars() 
      # myPars$cex<-2
      # mychartTheme <- chart_theme()

            # xts(temp[,.(open,high,low,close,volume)],order.by =temp$date)  %>%
      #   chart_Series(TA='addBBands();'
      #               #TA='addRSI(on=1)'
      #               ,theme = "white"
      #               #,theme = mychartTheme
      #               #,pars = myPars
      #               )
      
      
    },height = PLOT_HEIGHT*1.5)
    
  })
  
  
  
  

      
}#end Server







