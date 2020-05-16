#UI and Server function definitions for the Technical Analysis page

###############     Called from ui.R      ################
fnUI_technicalAnalysis <- function(){
    
    fluidPage(fluidRow(box(width = 12,solidHeader = T,background = "navy"
                           ,column(width = 2,h3("Technical Analysis for :"))
                           ,column(width = 3,uiOutput("lovCharts_all")))
                       )
              ,fluidRow(column(width = 6,style='padding:0px;margin:0px;'
                               ,box(width = 12,solidHeader = T,background = "navy",height = PLOT_HEIGHT*1.7
                                    ,plotOutput('plotCharts_technicalChart')
                                    ,title = "Technical Indicators"))
                        ,column(width = 6
                                ,fluidRow(column(width=12,style='padding:0px;'
                                                 ,box(width = 12,solidHeader = T,background = "navy"
                                                      ,title = "Prices for the last six sessions + Highs and Lows"
                                                      ,tableHTML_output('charts_tblKPI')))
                                          ,column(width=12,style='padding:0px;'
                                                  ,box(width = 12,solidHeader = T,background = "navy"
                                                       ,collapsible = T,collapsed = F
                                                       ,span(htmlOutput('txt_chartingNotes'), style="color:white;font-size:11px")
                                                       ,style = paste("height:",PLOT_HEIGHT*1.2,"px;overflow-y: scroll;",sep="")
                                                       ,title = "Notes on Technical Indicators"))
                                          ,column(width = 12,style='padding:0px;margin:0px;'
                                                  ,box(width = 12,solidHeader = T,background = "navy"
                                                       #,height = PLOT_HEIGHT*0.9
                                                       ,collapsible = T,collapsed = T
                                                       ,title = "Candlestick Cheat sheet"
                                                       ,imageOutput('charts_Image')))
                                          ,column(width=12,style='padding:0px;'
                                                  ,box(width = 12,solidHeader = F,collapsible = T,collapsed = T
                                                       ,title = "OHLC Data"
                                                       ,dataTableOutput('charts_tblOHLCdata')))
                                          )
                                )
                        )
    )
}


###############     Called from server.R      ################
fnServer_technicalAnalysis <- function(input,output,session){
    
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
    
    
    output$txt_chartingNotes <- renderText({ read_file(here::here(DATA_FOLDER,'charting-notes.html')) })
    
    output$charts_Image <- renderImage({
        return(list(src = here::here(DATA_FOLDER,"Candlestick-Cheat-Sheet_web-01.jpg")
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
}