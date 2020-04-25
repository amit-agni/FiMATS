
fnUI_dataRefresh <- function(){
    
    verticalLayout(
        box(collapsible = T,solidHeader = T,status = "primary",title ="Data Storage"
            ,radioButtons('radio_newSavedData',label="Data Load"
                          ,choiceValues = list("new","saved")
                          ,choiceNames = list("Load fresh data using symbols in CSV file"
                                               ,"Load previously saved data and update it if needed")
                          ,selected = "saved"
                          )
            ,conditionalPanel(
                condition = "input.radio_newSavedData == 'new'"
                ,dateInput("date_dataStartDate","Start date for loading the data",format = "dd-M-yy")
                ,fileInput("file_yahooCodes", "CSV File with symbols as in Yahoo"
                                ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                ,width = '100%')
                     # ,checkboxGroupInput('chkb_loadWhat'
                     #                     ,label = "For the symbols in the selected file, get"
                     #                     ,choices = c("Daily historical values","Financial Stats")
                     #                     ,inline = F)
                ,"Note : The file should be in CSV format and must have these columns : country, category, sector, symbol, name. 
            All the columns except name are mandatory"
                ,br()
                ,br()
                ,radioButtons('radio_saveDataYN',label="Save data for future use (needs Dropbox)"
                              ,choices = c("Yes","No"),selected = "No",inline = T)
                ,actionButton("ab_loadFreshData","Load")
            )
            ,conditionalPanel(
                condition = "input.radio_newSavedData == 'saved'"
                ,actionButton("ab_loadSavedData","Load")
                ,br()
                ,br()
                ,textOutput('txt_histDataStatus')
                ,br()
                ,br()
                )
            )
        ,box(collapsible = T,solidHeader = T,status = "primary",title ="Data for My Shares"
             ,radioButtons('radio_newSavedMyShares',label="Data Load"
                           ,choiceValues = list("new","saved")
                           ,choiceNames = list("Load new data from CSV file"
                                               ,"Load previously saved data")
                           ,selected = "saved"
             )
             ,conditionalPanel(
                 condition = "input.radio_newSavedMyShares == 'new'"
                 ,fileInput("file_myShares", "CSV File with my Shares"
                            ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                            ,width = '100%')
                 ,"Note : The file should be in CSV format and must have these columns : symbol, transaction_date, transaction_type, price, qty
            All the columns except qty are mandatory"
                 ,br()
                 ,br()
                 ,radioButtons('radio_saveDataYNMyShares',label="Save data for future use (needs Dropbox)"
                               ,choices = c("Yes","No"),selected = "No",inline = T)
                 ,actionButton("ab_loadMySharesNew","Load")
             )
             ,conditionalPanel(
                 condition = "input.radio_newSavedMyShares == 'saved'"
                 ,actionButton("ab_loadMySharesSaved","Load")
                 ,br()
                 ,br()
             )
        )
    )
    
# ,tags$style(make_css(list('.well', 'border-width', '0px')))
# ,tags$style(make_css(list('.well', 'background-color', 'transparent')))
}


fnUI_eagleEye <- function(){
    
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


fnUI_myShares <- function(){
    
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


fnUI_deepAnalyse <- function(){
    
    fluidPage(
        fluidRow(column(width = 12
                        ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="KPIs"
                             ,tableOutput("tblDeep_KPI"))))
        ,fluidRow(column(width = 2
                         ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Additional Parameters"
                              ,uiOutput("lovDeep_all")
                              ,radioButtons('radioDeep_PlotYTDType',label = 'Plot Type',choices = c("Standard","CandleStick","OHLC","Waterfall","Funnel","FunnelArea","Indicator"))))
                  ,column(width = 7,uiOutput("boxDeep_PlotYTD"))
                  ,column(width = 3
                          ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Real Time"
                               ,plotOutput("plotDeep_realTime",height = PLOT_HEIGHT)))
        )
         #Disable flickering during Refresh
         ,tags$style(type="text/css", "#boxDeep_PlotYTD.recalculating { opacity: 1.0; }")
         ,tags$style(type="text/css", "#plotDeep_realTime.recalculating { opacity: 1.0; }")
         

    )

}




fnUI_charts <- function(){
    
    fluidPage(fluidRow(box(width = 12,solidHeader = T,background = "navy"
                           ,column(width = 2,h3("Technical Analysis for :"))
                           ,column(width = 3,uiOutput("lovCharts_all")))
                       )
              ,fluidRow(column(width = 6
                                ,style='padding:0px;margin:0px;'
                                ,box(width = 12,solidHeader = T,background = "navy",height = PLOT_HEIGHT*1.7
                                     ,plotOutput('plotCharts_technicalChart')
                                     ,title = "Technical Indicators"))
                        ,column(width = 6
                                ,fluidRow(column(width=12
                                                 ,style='padding:0px;'
                                                 ,box(width = 12,solidHeader = T,background = "navy"
                                                      ,title = "Prices for the last six sessions + Highs and Lows"
                                                      ,tableHTML_output('charts_tblKPI')))
                                          ,column(width=12
                                                  ,style='padding:0px;'
                                                  ,box(width = 12,solidHeader = T,background = "navy"
                                                       ,collapsible = T,collapsed = T
                                                       #,htmlOutput('txt_chartingNotes')
                                                       ,span(htmlOutput('txt_chartingNotes'), style="color:white;font-size:11px")
                                                       ,style = paste("height:",PLOT_HEIGHT*1.2,"px;overflow-y: scroll;",sep="")
                                                       ,title = "Notes on Technical Indicators"))
                                          ,column(width = 12
                                                  ,style='padding:0px;margin:0px;'
                                                  ,box(width = 12,solidHeader = T,background = "navy"
                                                       #,height = PLOT_HEIGHT*0.9
                                                       ,collapsible = T,collapsed = T
                                                       ,title = "Candlestick Cheat sheet"
                                                       ,imageOutput('charts_Image')))
                                          ,column(width=12
                                                  ,style='padding:0px;'
                                                  ,box(width = 12
                                                       ,solidHeader = F
                                                       #,background = "navy"
                                                       ,collapsible = T,collapsed = T
                                                       ,title = "OHLC Data"
                                                       ,dataTableOutput('charts_tblOHLCdata')))
                                          )
                                )
                        )
              # ,fluidRow(column(width = 12
              #                   ,style='padding:0px;margin:0px;'
              #                   ,box(width = 12,solidHeader = T,background = "navy"
              #                        ,collapsible = T,collapsed = T
              #                        ,title = "Candlestick chart cheat sheet"
              #                        ,imageOutput('charts_Image'))))
              )
    
    
}


fnUI_opportunities <- function(){
    
    fluidPage(tabBox(id='oppTabBox'
                     ,width = 12
                     ,title = "Experimental Features (Australian stocks only)"
                     ,side = "left"
                     ,tabPanel(value = "tab1",title = "Risk Reward"
                               ,h3("Risks vs Rewards for the selected period")
                               ,p("Reward is the mean of the log of the daily returns in the selected period and Risk is the corresponding standard deviation")
                               
                               ,fluidRow(box(width = 12,solidHeader = T,background = "olive"
                                             ,column(width=4,uiOutput('riskReward_sectorLov'))
                                             ,column(width=4,uiOutput('riskReward_excludeStocksLov'))))
                               ,fluidRow(column(width = 8,plotOutput('riskReward_plot',height = PLOT_HEIGHT*1.5))
                                         ,column(width = 4,tableHTML_output('riskReward_table')))

                     )
                     ,tabPanel(value = "tab2",title = "Sector Correlations"
                               ,h3("Correlation of Daily log returns - Sectorwise")
                               #,fluidRow(column(width=12,h4(textOutput('sectorCorrelations_text'))))
                               ,strong(textOutput('sectorCorrelations_text1'))
                               ,column(width=8,style='padding:0px;'
                                    ,p("The sector price is a simple mean of the stock prices in that sector.
                                    The log of the daily returns is then computed for all the dates in the specified period.
                                        The correlation plot shows the how the sector price movements are related"
                                        ,textOutput('sectorCorrelations_text2')))
                               ,br()
                               ,br()
                               ,br()
                               ,br()
                               ,fluidRow(column(width=5,plotOutput('sectorCorrelations_plot',height = PLOT_HEIGHT))
                                         ,column(width=7,tableHTML_output('sectorCorrelations_table')))
                               )
                     
                     
                     
                      ,tabPanel(value = "tab3",title = "Simulations"
                                ,h3("Monte Carlo Simulations")
                                ,fluidRow(box(width = 12,solidHeader = T,background = "olive"
                                              ,column(width=3,uiOutput('monteCarlo_stockLOV'))
                                              ,column(width = 3,radioButtons('monteCarlo_type'
                                                                         ,label="Simulation Type"
                                                                         ,choices = c("Random Walk","Brownian Motion")
                                                                         ,inline = T))
                                              ,column(width = 3,sliderInput('monteCarlo_sliderForecastPeriod'
                                                                        ,label="Forecast horizon:"
                                                                        ,min = 1,max = 1000, value = 100))
                                              ,column(width = 3,sliderInput('monteCarlo_sliderSimulations'
                                                                        ,label="Number of Simulations:"
                                                                        ,min = 1,max = 5000, value = 100))
                                )
                                          
                                )
                                ,fluidRow(column(width=12,h4(strong(textOutput('monteCarlo_expectedPrice'))))
                                          ,column(width=4,
                                                 fluidRow(column(width=12
                                                                  ,plotOutput('monteCarlo_historgram'))
                                                          ,column(width=12
                                                                  ,br()
                                                                  ,br()
                                                                  ,br()
                                                                  ,br()
                                                                  ,br()
                                                                  ,strong("Confidence Intervals for the simulated stock prices")
                                                                  ,p("We can say with 95% confidence that the stock price will lie between the values shown under 5% and 95% below")
                                                                  ,tableHTML_output('monteCarlo_probabilities'))))
                                          ,column(width = 8,plotOutput('monteCarlo_simulationsPlot')))
                                )
                      ,tabPanel(value = "tab4",title = "Buy Sell Trading Strategy")
                     
                     )
              
              )
}


#ERROR: Invalid color: grey. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal
#, olive, lime, orange, fuchsia, purple, maroon, black.










