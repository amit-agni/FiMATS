server <- function(input,output,session){
    
    #####################
    #   Data Refresh   #
    #####################
    observeEvent(input$ab_initialLoad,fnSrv_initialLoad(input,output,session))
    observeEvent(input$ab_summaryStats,fnSrv_summaryStats(input,output,session))
    observeEvent(input$ab_dataCatchup,fnSrv_dataCatchup(input,output,session))
    observeEvent(input$ab_loadMyShares,fnSrv_loadMyShares(input,output,session))
    
    #Realtime data
    DT_realTime=reactiveVal(data.frame())
    observe({
      print(input$radio_realTimeYN)
      if(input$radio_realTimeYN=="Yes"){
        invalidateLater(input$slider_realTimeDelay*1000)
        isolate({
          DT_realTime(rbind(DT_realTime(),fnSrv_realTimeData(DT_stats())))
        })
        }
      })
    
    
    #####################
    #   Overall View     #
    #####################
    
    fn_srvEaglePopulateLOV(input,output,session,DT_stats())
    
    observe(
    {
      
      req(input$radioEagle_selectCategory)
      varEagle_parameters <- case_when(input$lovEagle_selectCountry == 'ALL' & input$lovEagle_selectSector == 'ALL' ~ 
                                         "category==input$radioEagle_selectCategory"
                                       ,input$lovEagle_selectCountry == 'ALL' & input$lovEagle_selectSector != 'ALL' ~ 
                                         "sector==input$lovEagle_selectSector & category==input$radioEagle_selectCategory"
                                       ,input$lovEagle_selectCountry != 'ALL' & input$lovEagle_selectSector == 'ALL' ~ 
                                         "country==input$lovEagle_selectCountry & category==input$radioEagle_selectCategory"
                                       ,input$lovEagle_selectCountry != 'ALL' & input$lovEagle_selectSector != 'ALL' ~ 
                                         "country==input$lovEagle_selectCountry & sector==input$lovEagle_selectSector & category==input$radioEagle_selectCategory")
      
      #!is.na(close) 
      
      #Value boxes
      output$valueBoxes_eagle <- renderUI({
        
        if(input$radio_realTimeYN=="Yes"){
          
          temp_vb <- merge(DT_realTime()[eval(parse(text=varEagle_parameters)),.SD[which.max(`Trade Time`)],.(symbol,category)]
                           ,DT_hist()[,.SD[which.max(date)],symbol][,.(symbol,close)] #need for growth over previous day 
                           ,all.x=T,by="symbol")[,.(symbol,category,Last,name,growth=(Last/close)-1)][,head(.SD,NO_OF_VALUE_BOXES)]
          setnames(temp_vb,"Last","value")
        }else{
          #browser()
          temp_vb <- DT_hist()[eval(parse(text=varEagle_parameters))
                               ,lapply(.SD[order(-date)],function(x) head(x,2)),symbol][
            ,.(date,name,close,prev_close=shift(close,n=1L,type="lead")
               ,growth=(close/shift(close,n=1L,type="lead"))-1),symbol][!is.na(prev_close)][,head(.SD,NO_OF_VALUE_BOXES)]
          setnames(temp_vb,"close","value")
          
          temp_vb <- temp_vb[!is.na(value)]
          
        }
        
        lapply(1:nrow(temp_vb),function(i) {
          icon <- if(temp_vb[i]$growth < 0){ icon("thumbs-down") }else{ icon("thumbs-up") }
          color <- if(temp_vb[i]$growth < 0){ "red" }else{ "green" }
          valueBox(tags$p(format(round(temp_vb[i]$value,0),big.mark=","),style ="font-size:60%;")
                   ,tags$p(paste(temp_vb[i]$name," ",round(100*temp_vb[i]$growth,2),"%",sep=""),style ="font-size:80%;")
                   ,icon=NULL
                   ,color = color
                   ,width = 1) 
        })
      })
      

        var_plot <- reactive({ if(input$radio_realTimeYN=="Yes"){
          fn_plotRealTime(DT_realTime = DT_realTime()[eval(parse(text=varEagle_parameters))]
                          ,varSymbols=NULL
                          ,DT_myShares=NULL
                          ,displayPerPage=input$radioEagle_displayPerPage)
          }else{
            fn_plotYTD(DT_hist=DT_hist()[eval(parse(text=varEagle_parameters))]
                       ,dt_start=input$dt_start
                       ,dt_end=input$dt_end
                       ,varSymbols=NULL
                       ,DT_myShares=NULL
                       ,displayPerPage=input$radioEagle_displayPerPage)
            }
          })


        var_dynamicHeight <- reactive(gg_facet_nrow(var_plot()))

        output$box_plotEagle <- renderUI({

          box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = input$radioEagle_selectCategory
              ,renderPlot({ var_plot() },height = function(){var_dynamicHeight()*FACET_ROW_HEIGHT}))
          
        })

      

    }) #end observe
    

    
    
    #################
    #   My Shares   #
    #################
    
    observeEvent(input$menuTabs,
     {
      
       input$radio_realTimeYN
       
       #Value boxes
       output$valueBoxes_myShares <- renderUI({
         if(input$radio_realTimeYN=="Yes"){
           
           temp_vb_myShares <- merge(DT_realTime()[,.SD[which.max(`Trade Time`)],symbol]
                            ,DT_hist()[,.SD[which.max(date)],symbol][,.(symbol,close)]
                            ,all.x=T,by="symbol")[,.(symbol,category,Last,name,growth=(Last/close)-1)][
                              category=='stock'][symbol %in% unique(DT_myShares()$symbol)]
           
           setnames(temp_vb_myShares,"Last","value")
         }else{
           temp_vb_myShares <- DT_hist()[category=="stock" & !is.na(close),lapply(.SD[order(-date)],function(x) head(x,2)),symbol][
             ,.(date,name,close,prev_close=shift(close,n=1L,type="lead")
                ,growth=(close/shift(close,n=1L,type="lead"))-1),symbol][!is.na(prev_close)][symbol %in% unique(DT_myShares()$symbol)]
           setnames(temp_vb_myShares,"close","value")
           
         }
         lapply(1:nrow(temp_vb_myShares),function(i) {
           icon <- if(temp_vb_myShares[i]$growth < 0){ icon("thumbs-down") }else{ icon("thumbs-up") }
           color <- if(temp_vb_myShares[i]$growth < 0){ "red" }else{ "green" }
           valueBox(value=tags$p(format(round(temp_vb_myShares[i]$value,1),big.mark=","),style ="font-size:60%;")
                    ,subtitle = tags$p(paste(temp_vb_myShares[i]$name," ",round(100*temp_vb_myShares[i]$growth,2),"%",sep=""),style ="font-size:80%;")
                    ,icon=NULL
                    ,color = color
                    ,width = 1)
         })
       })

       #Plots
       
       #Static height
       # output$plotmyShares <- renderPlot({ 
       #   if(input$radio_realTimeYN=="Yes"){
       #     fn_plotRealTime(DT_realTime=DT_realTime()
       #                     ,DT_stats=DT_stats()
       #                     ,varSymbols=NULL
       #                     ,DT_myShares=DT_myShares()) 
       #     }else{
       #       fn_plotYTD(DT_hist=DT_hist()
       #                               ,dt_start=input$dt_start
       #                               ,dt_end=input$dt_end
       #                               ,varSymbols=NULL
       #                               ,DT_myShares=DT_myShares()) 
       #       }
       # })
       
       
       #Dynamic height plot inside a box
       var_plot <- reactive({
         if(input$radio_realTimeYN=="Yes"){
           fn_plotRealTime(DT_realTime=DT_realTime()
                                       #,DT_stats=DT_stats()
                                       ,varSymbols=NULL
                                       ,DT_myShares=DT_myShares()) 
         }else{
           fn_plotYTD(DT_hist=DT_hist()
                                  ,dt_start=input$dt_start
                                  ,dt_end=input$dt_end
                                  ,varSymbols=NULL
                                  ,DT_myShares=DT_myShares()) 
         }
         
       })
         
       var_dynamicHeight <- reactive(gg_facet_nrow(var_plot()))
       
       output$box_mySharesPlot <- renderUI({
         
         box(collapsible = T,solidHeader = T,width = NULL,status = "info",title ="My Shares"
             ,renderPlot({ var_plot() },height = function(){var_dynamicHeight()*FACET_ROW_HEIGHT}))
         
       })
       
       
     }) #end observe
    
    
    

    ######################
    #   Deep Analysis    #
    ######################
    fn_srvDeepPopulateLOV(input,output,session,DT_stats())
    

    observeEvent(input$lovDeep_all,{ 
      req(input$lovDeep_all)
      output$tblDeep_KPI <- renderTable({ fn_tblKPI(DT_hist(),DT_stats(),input$lovDeep_all) })
      output$plotDeep_YTDFcst <- renderPlot({ fn_plotYTD(DT_hist=DT_hist()
                                                         ,dt_start=input$dt_start
                                                         ,dt_end=input$dt_end
                                                         ,varSymbols=input$lovDeep_all
                                                         ,DT_myShares=DT_myShares()) }
                                            ,height = PLOT_HEIGHT)

      output$plotDeep_realTime <- renderPlot({ if(input$radio_realTimeYN=="Yes"){
        fn_plotRealTime(DT_realTime=DT_realTime()
                        #,DT_stats=DT_stats()
                        ,varSymbols=input$lovDeep_all
                        ,DT_myShares=NULL) 
        }
        }
        ,height = PLOT_HEIGHT)
      
    })
    

}


