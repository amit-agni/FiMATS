server <- function(input, output, session) {
  
  
  #####################
  #   Overall View    #
  #####################
  
  
  output$boxEagle_additionalParameters <- renderUI({
    validate(need(DT_stats()$category,"Data is not loaded, please load data using the Data Processing Page"))
    
    box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "Additional Parameters"
        ,radioButtons("radioOverview_selectCategory",label = "Category",choices = unique(DT_stats()$category))
        ,selectInput("lovOverview_selectCountry",label = "Country",choices = c('ALL', unique(DT_stats()$country)))
        ,selectInput("lovOverview_selectSector",label = "Sector",choices = c('ALL', unique(DT_stats()$sector)))
        ,radioButtons("radioEagle_displayPerPage",label = "Display per page",choices = c(8,12,24, "ALL"),inline = T,selected = "12")
    )
    })
  
  
  observeEvent(input$radioOverview_selectCategory, {
    req(DT_stats())
    
    req(input$radioOverview_selectCategory)
    req(input$lovOverview_selectCountry)
    
    input$radioOverview_selectCategory
    input$lovOverview_selectCountry
    
    
    updateSelectInput(session,inputId = "lovOverview_selectCountry",
                      choices = c('ALL', unique(DT_stats()[category == input$radioOverview_selectCategory]$country)))
    updateSelectInput(session,inputId = "lovOverview_selectSector",
                      choices = c('ALL', unique(DT_stats()[category == input$radioOverview_selectCategory]$sector)))
    })
  
  observe({
    req(DT_stats())
    
    varEagle_parameters <-
      case_when(input$lovOverview_selectCountry == 'ALL' & input$lovOverview_selectSector == 'ALL' ~
                            "category==input$radioOverview_selectCategory"
                          ,input$lovOverview_selectCountry == 'ALL' & input$lovOverview_selectSector != 'ALL' ~
                            "sector==input$lovOverview_selectSector & category==input$radioOverview_selectCategory"
                          ,input$lovOverview_selectCountry != 'ALL' & input$lovOverview_selectSector == 'ALL' ~
                            "country==input$lovOverview_selectCountry & category==input$radioOverview_selectCategory"
                          ,input$lovOverview_selectCountry != 'ALL' & input$lovOverview_selectSector != 'ALL' ~
                            "country==input$lovOverview_selectCountry & sector==input$lovOverview_selectSector & category==input$radioOverview_selectCategory"
                          
      )
    
    #Value boxes
    output$valueBoxes_eagle <- renderUI({
      if(input$radioOverview_selectCategory != 'Sector' & input$lovOverview_selectSector != 'ALL'){
        NO_OF_VALUE_BOXES <- Inf
        }
      
      if(input$radio_realTimeYN == "Yes") {
        req(DT_realTime())
        req(DT_hist())
        temp_vb <- merge(DT_realTime()[eval(parse(text = varEagle_parameters)), .SD[which.max(`Trade Time`)], .(symbol, category)]
                         ,DT_hist()[, .SD[which.max(date)], symbol][, .(symbol, close)] #need for growth over previous day
                         ,all.x = T,by = "symbol")[
                           , .(symbol, category, Last, name, growth = (Last / close) - 1)][, head(.SD, NO_OF_VALUE_BOXES)]
        setnames(temp_vb, "Last", "value")
        }else{
          req(DT_hist())
          temp_vb <- DT_hist()[eval(parse(text = varEagle_parameters))
                               ,lapply(.SD[order(-date)], function(x) head(x, 2)), symbol][
                                 , .(date,name,close,prev_close = shift(close, n = 1L, type = "lead")
                                     ,growth = (close / shift(close, n = 1L, type = "lead")) - 1), symbol][
                                       !is.na(prev_close)][, head(.SD, NO_OF_VALUE_BOXES)]
          setnames(temp_vb, "close", "value")
      }
      
      
      temp_vb <- temp_vb[!is.na(value) & !is.na(growth)]
      setorder(temp_vb,name)
      
      lapply(1:nrow(temp_vb), function(i) {
        icon <-if (temp_vb[i]$growth < 0) {icon("thumbs-down")} else{icon("thumbs-up")}
        color <- if (temp_vb[i]$growth < 0) {"red"} else{"green"}
        valueBox(tags$p(format(round(temp_vb[i]$value, 1), big.mark = ","), style = "font-size:60%;")
                 ,tags$p(paste(temp_vb[i]$name," ",round(100 * temp_vb[i]$growth, 2),"%",sep = "")
                         , style = "font-size:80%;")
          ,icon = NULL,color = color,width = 1
        )
      })
    }) #end render value boxes
    
    
    var_plot <- reactive({
      if (input$radio_realTimeYN == "Yes") {
        req(DT_realTime())
        fn_plotRealTime(DT_realTime = DT_realTime()[eval(parse(text = varEagle_parameters))]
                        ,varSymbols = NULL,DT_myShares = NULL,displayPerPage = input$radioEagle_displayPerPage
        )
      }else{
        req(DT_hist())
        fn_plotYTD(DT_hist = DT_hist()[eval(parse(text = varEagle_parameters))]
                   ,dt_start = input$dt_start,dt_end = input$dt_end,varSymbols = NULL,DT_myShares = NULL
                   ,displayPerPage = input$radioEagle_displayPerPage
        )
      }
    })
    
    var_dynamicHeight <- reactive(gg_facet_nrow(var_plot()))
    output$box_plotEagle <- renderUI({
      box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = input$radioOverview_selectCategory
        ,renderPlot({var_plot()}, height = function() {var_dynamicHeight() * FACET_ROW_HEIGHT})
      )
      
    })
    
    #DT_realTimeWinnersLosers <- reactive({ })
    
    output$tblEagle_Winners <- renderDataTable({ 
      
      req(DT_realTime())
      
      if(input$radio_realTimeYN == "Yes") {
        
        isolate({
          fn_tblWinnersLosers(DT_hist=DT_hist()[eval(parse(text = varEagle_parameters))][
            ,lapply(.SD, function(x) tail(x,1)),symbol]
            ,DT_realTime = DT_realTime()[eval(parse(text = varEagle_parameters))][
              ,lapply(.SD, function(x) tail(x,1)),symbol]) %>%
            datatable(rownames = F) %>%
            formatRound(.,c(3:5), 1) 
          })
      }
      },options=list(pageLength = 5
                       #,lengthMenu = c(2, 12, 18)
                       ,searching= FALSE
                       ,columnDefs = list(list(className = 'dt-center'))
                       ,rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}")
                       #,class="compact"
                       ,class = 'white-space: nowrap stripe hover')
      )
    
  })#end observe overall view
  
  
  
  
  ##############################
  #   My Shares (Invesments)   #
  ##############################
  
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
  
  
  

  
  #########################
  #   Technical analysis  #
  #########################
  
  output$lovCharts_all <- renderUI({
    req(DT_stats()$category)
    selectInput("lovCharts_all",label = ""
                #,choices = split(DT_stats()[,.N,.(category,name)][,-"N"],by="category",keep.by=F,flatten=F)
                ,choices = list("Stocks" = DT_stats()[category == 'stock']$name
                                ,"Indices" = DT_stats()[category == 'index']$name
                                ,"Currencies" = DT_stats()[category == 'currency']$name
                                ,"Commodities" = DT_stats()[category == 'commodity']$name)
                ,selected = DT_stats()[category=='stock' & country == 'Australia'][1, ]$name)
  })
  
  
  output$charts_tblKPI <- render_tableHTML({
    req(input$lovCharts_all)

    fn_tblKPI(DT_hist(), DT_stats(), input$lovCharts_all) %>% 
      tableHTML(rownames = F,border=2,widths = rep(100,10),spacing = "10px")
    #%>% add_theme('rshiny-blue')
    })
  
  output$charts_tblOHLCdata <- renderDataTable({
    req(input$lovCharts_all)
    
    datatable(DT_hist()[name == input$lovCharts_all & date >= input$dt_start & date <= input$dt_end][
      order(-date)][,.(date,open,high,low,close,volume,adjusted)],rownames = F) %>%
      formatRound(.,c(2:8), 0) 
    #   formatStyle(columns = c(1:3), 'text-align' = 'center')
    },options=list(pageLength = 5
                 #,lengthMenu = c(2, 12, 18)
                 ,searching= FALSE
                 ,columnDefs = list(list(className = 'dt-center'))
                 ,rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}")
                 #,class="compact"
                 #,class = 'white-space: nowrap stripe hover'
                 )
  ) 
  
  
  output$txt_chartingNotes <- renderText({ read_file(here::here('100_data_raw-input','charting-notes.html')) })
  
  output$charts_Image <- renderImage({
    return(list(src = here::here("100_data_raw-input","Candlestick-Cheat-Sheet_web-01.jpg")
         ,contentType = "image/jpg"
         ,height = "100%"
         ,width = "100%"))
    
    },deleteFile = F)
  
  
  observeEvent(input$lovCharts_all,{
  
    print("in chart")
    req(input$lovCharts_all)
    
    output$plotCharts_technicalChart <- renderPlot({
      
      temp <- DT_hist()[name == input$lovCharts_all & date >= input$dt_start & date <= input$dt_end]
      xts(temp[,.(open,high,low,close,volume)],order.by =temp$date)  %>%
        chartSeries(TA='addBBands();addRSI();addMACD();addVo()'
                    ,theme = "white"
                    ,multi.col = F
                    ,up.col="white"
                    ,dn.col ="darkslategray1")
                    #name = 'Technical Charts')
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
  
  
  #########################
  #   Labs - Experiments  #
  #########################
  
  
  ######################     1. Correlations     #######################
  
  output$sectorCorrelations_text1 <- renderText({
    paste("Period :",strftime(input$dt_start,format = '%d%b%y')," - ",strftime(input$dt_end,format = '%d%b%y'),sep="")
    })
  
  
  output$sectorCorrelations_text2 <- renderText({
    x <- fn_corTopBottom(correl())
    paste('For example : The daily price returns of '
          ,x[order(-value)][1]$var1
          ,' and '
          ,x[order(-value)][1]$var2
          ,' shows a high correlation of '
          ,round(x[order(-value)][1]$value,2)
          ,' for the selected period'
          ,sep="")
  })
  
  correl <- reactive({
    fn_opp_SectorCorrelations(DT_hist()[country == 'Australia' & category == 'stock'
                                                & date >= input$dt_start & date <= input$dt_end])
  })
  
  output$sectorCorrelations_plot <- renderPlot({
    ggcorrplot::ggcorrplot(correl(), hc.order = F, type = "lower",lab=TRUE,lab_size = 4)
                           #,title = "Log of Daily Returns - Sector Correlations")
    # +
    #   theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),  # Order: top, right, bottom, left
    #         axis.text.y = element_text(margin=margin(0,-2,0,0)))
    # 
    # https://stackoverflow.com/questions/41269593/customize-ggcorrplot
    # ggplot(melt(correl()), aes(Var1, Var2, fill=value)) +
    #   geom_tile(height=0.8, width=0.8) +
    #   scale_fill_gradient2(low="blue", mid="white", high="red") +
    #   theme_minimal() +
    #   coord_equal() +
    #   labs(x="",y="",fill="Corr") +
    #   theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1,
    #                                  margin=margin(-3,0,0,0)),
    #         axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
    #         panel.grid.major=element_blank())
    
    })
  
  
  output$sectorCorrelations_table <- render_tableHTML({
    
    x <- fn_corTopBottom(correl())
    
    x[,value:=round(value,2)]
    
    setnames(x,c('var1','var2','value'),c('Sector 1','Sector 2','Correlation Coefficient'))
    rbind(x[order(-`Correlation Coefficient`)][1:10]
          ,x[order(`Correlation Coefficient`)][1:10][order(-`Correlation Coefficient`)]
          ,fill=T) %>% 
      tableHTML(row_groups = list(c(10, 10), c('Top Ten', 'Bottom Ten'))
                ,widths = c(100,180,180,80)
                ,rownames = F
                ,spacing = '4px') %>%
      add_theme('rshiny-blue')
    })
  
  
  
  
  ######################### 2. Risk Rewards #########################
  
  output$riskReward_sectorLov <- renderUI({
    selectInput("riskReward_sectorLov",label = "Choose Sector",choices = c('ALL',unique(DT_stats()[category == 'stock']$sector)))
    })
  
  
  
  output$riskReward_excludeStocksLov <- renderUI({
    selectInput("riskReward_excludeStocksLov",label = "Exclude Stocks"
                ,choices = unique(DT_stats()[category == 'stock']$name)
                ,multiple = T)
  })
  
  
  DT_riskReward <- reactive({ 
    req(input$riskReward_sectorLov )
    if(input$riskReward_sectorLov == 'ALL'){
      fn_opp_RiskReward(DT_hist()[country == 'Australia' & category == 'stock' 
                                  & !name %in% input$riskReward_excludeStocksLov
                                  & date >= input$dt_start & date <= input$dt_end])  
    }else{
      fn_opp_RiskReward(DT_hist()[country == 'Australia' & category == 'stock' 
                                  & !name %in% input$riskReward_excludeStocksLov
                                  & sector == input$riskReward_sectorLov
                                  & date >= input$dt_start & date <= input$dt_end])  
    }
    
  })
  
  output$riskReward_plot <- renderPlot({
    DT_riskReward() %>% 
      ggplot(aes(x=risk,y=reward)) +
      geom_point(aes(color=risk,size=reward)) +
      facet_wrap(~sector,ncol=3) +
      geom_text_repel(aes(label=name),size=4) + #,scales="free") 
      theme(strip.text.x = element_text(size = 12)
            ,axis.title = element_text(size = 16)
            ,axis.text = element_text(size = 16)) +
      scale_color_gradient(low = "green", high = "red", na.value = NA)
    
    })
  
  output$riskReward_table <- render_tableHTML({
    
    DT_riskReward() %>% 
      tableHTML(rownames = F
                ,widths = c(80,180,180,80,80)) %>%
      add_theme('rshiny-blue')
    
    # row_groups = list(c(10, 10), c('Top Ten', 'Bottom Ten'))
    #             ,widths = c(100,180,180,80)
    #             ,rownames = F
    #             ,spacing = '4px') %>%
    #   add_theme('rshiny-blue')
    
  })
  
  ######################### 3.  Monte Carlo   #########################
  output$monteCarlo_stockLOV <- renderUI({
    selectInput("monteCarlo_stockLOV",label = "Choose Stock",choices = DT_stats()[category == 'stock']$name)
  })
  
  
  DT_monteCarlo <- reactive({
    req(input$monteCarlo_stockLOV)
    temp <- DT_hist()[name==input$monteCarlo_stockLOV,.(category,symbol,date,close,sector,country,name)]
    n <- 1L
    temp[,`:=`(close_log = log(close)
               ,close_shift = shift(close,n=n,type = "lag")
               ,close_shiftlog = shift(log(close),n=n,type = "lag")
               ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
         ,by = symbol]
    # temp[,`:=`(close_log = (close)
    #            ,close_shift = shift(close,n=n,type = "lag")
    #            ,close_shiftlog = shift((close),n=n,type = "lag")
    #            ,close_DiffLagLog = (close-shift((close),n=n,type = "lag"))/shift((close),n=n,type = "lag")
    # ),by = symbol]
    
    temp
  })
  
  observe({
    mean <- mean(DT_monteCarlo()$close_DiffLagLog,na.rm = T)
    sd <- sd(DT_monteCarlo()$close_DiffLagLog,na.rm = T)
    price <- DT_monteCarlo()[order(-date)][1]$close
    
    days <- input$monteCarlo_sliderForecastPeriod
    simulations <- input$monteCarlo_sliderSimulations
    
    
    if(input$monteCarlo_type =='Random Walk'){
      matrix <- matrix(NA,nrow=days,ncol=simulations)
      for(col in 1:ncol(matrix)){
        matrix[1,col]<-price
        for(row in 2:nrow(matrix)){
          matrix[row,col] <- fn_randomWalk(matrix[row-1,col],mean,sd)
        }
      }
      
    }else{
      matrix <- matrix(NA,nrow=days,ncol=simulations)
      for(col in 1:ncol(matrix)){
        matrix[1,col]<-price
        for(row in 2:nrow(matrix)){
          matrix[row,col] <- fn_brownianMotion(matrix[row-1,col],row,mean,sd)
        }
      }
      
    }
    
    DT <- fn_mungeMatrix(matrix)
    
    output$monteCarlo_expectedPrice <- renderText({
      paste("Current Stock price:",round(price,1)
            ,". Simulated Stock price at the end of "
            ,days,"days is:",round(quantile(DT[session == max(DT$session)][,-1],probs = 0.5),1)
            ,"(Confidence probability : 50%)")
      
    })
    output$monteCarlo_simulationsPlot <- renderPlot({
      fn_plotSimulations(DT)  +
        labs(title = paste("Movement of stock prices for",days,"days over",simulations,"Monte Carlo simulations")
             ,ylab ="Stock price")
    },height=PLOT_HEIGHT)
    
    output$monteCarlo_historgram  <- renderPlot({
      DT[session == max(DT$session)] %>%
        melt(id.var="session") %>%
        ggplot(aes(x=value)) +
        geom_histogram(bins = 15) +
        cutlery::theme_darklightmix(color_theme = "lightcyan",legend_position = "bottom") +
        theme(strip.text.x = element_text(size = 14, colour = "black")
              ,plot.background = element_rect(fill = 'white')
              ,legend.position = "none") +
        labs(title = paste("Distribution of the simulated stock price after",max(DT$session),"days")
             ,subtitle = paste("Current Stock price:",round(price,1))
             ,xlab ="Stock price")
    },height=PLOT_HEIGHT)
    
    output$monteCarlo_probabilities <- render_tableHTML({
      quantile(DT[session == max(DT$session)][,-1]
               ,probs = c(0,0.05,0.25,0.5,0.75,0.95,1))  %>% 
        tableHTML(rownames = F
                  #,caption = "Probabilities"
                  ,round=1)
    })
    
    
  })
  
  
  
  #####################
  #   Data Refresh   #
  #####################
  
  DT_stats <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_stats.Rds")))
  DT_hist <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_hist.Rds")))
  DT_myShares <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_myShares.Rds")))
  DT_realTime <- reactiveVal(data.frame())
  
  observe({req(DT_hist()$date)
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
  
  
      
}#end Server







