#UI and Server function defintions for the Market Overview Page

###############     Called from ui.R      ################
fnUI_overview <- function(){
    
    fluidPage(
        fluidRow(column(width = 12,uiOutput("valueBoxes_eagle"))
                 ,fluidRow(
                     column(width=2,uiOutput("boxEagle_additionalParameters"))
                     ,column(width = 7,uiOutput("box_plotEagle")
                             ,tags$style(type="text/css", "#box_plotEagle.recalculating { opacity: 1.0; }"))
                     ,column(width=3,
                             box(collapsible = T,solidHeader = T,width = NULL,status = "info",title = "Winners and Losers (Enable RealTime)"
                                 ,dataTableOutput('tblEagle_Winners')))
                 )
        )
    )
}


###############     Called from server.R      ################
fnServer_overview <- function(input,output,session){
    
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
    
}

