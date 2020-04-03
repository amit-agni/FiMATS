
fnUI_dataRefresh <- function(){
    
    verticalLayout(
        box(collapsible = T,solidHeader = T,status = "primary",title ="Initial Data Loading"
            ,fileInput("file_yahooCodes", "CSV File with symbols as in Yahoo"
                                   ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                   ,width = '100%')
                        ,checkboxGroupInput('chkb_loadWhat'
                                            ,label = "For the symbols in the selected file, get"
                                            ,choices = c("Daily historical values","Financial Stats")
                                            ,inline = F)
                        ,actionButton("ab_initialLoad","Load"))
        ,box(collapsible = T,solidHeader = T,status = "primary",title ="Additional Data Processing"
             ,actionButton("ab_dataCatchup","Data catchup")
             ,actionButton("ab_summaryStats","Store Summary Stats")
        )
        ,box(collapsible = T,solidHeader = T,status = "primary",title ="Data for My Shares"
             ,fileInput("file_myShares", "CSV File with my Shares"
                                    ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                    ,width = '100%')
                         ,actionButton("ab_loadMyShares","Load"))
        ,box(collapsible = T,solidHeader = T,status = "primary",title ="Status"
             ,textOutput('txt_dataStatus'))
    )
    
# ,tags$style(make_css(list('.well', 'border-width', '0px')))
# ,tags$style(make_css(list('.well', 'background-color', 'transparent')))
}


fnUI_eagleEye <- function(){
    
    fluidPage(
        fluidRow(column(width = 12,uiOutput("valueBoxes_eagle"))
             ,fluidRow(
                    # column(width=2
                    #        ,box(
                    #            hr()                           
                    #            ,dateInput("dtEagle_start",label = "Start Date",value = "2019-01-01",format = "dd-M-yy")
                    #            ,dateInput("dtEagle_end",label = "End Date",format = "dd-M-yy")
                    #            ,hr()
                    #            ,checkboxGroupInput('chkbEagle_movAvgs',label = "Moving averages",choices = c("50-day MA","100-day MA","200-day MA"),inline = F)
                    #            ,br()
                    #            ,width = "100%"
                    #            ,status = "info"
                    #            ,solidHeader = T
                    #            ,title = "Parameter Selection")
                    #     )
                    column(width = 12
                            ,box(plotOutput("plotEagle_index",height = PLOT_HEIGHT)  %>%  withSpinner()
                                 ,collapsible = T,solidHeader = T,width = NULL,status = "info",title ="Indices")
                            ,fluidRow(
                                column(width = 6
                                       ,box(plotOutput("plotEagle_commodity",height = PLOT_HEIGHT * 0.7) %>%  withSpinner()
                                            ,collapsible = T,solidHeader = T,width = NULL,status = "info",title="Commodities"))
                                ,column(width = 6
                                        ,box(plotOutput("plotEagle_currency",height = PLOT_HEIGHT * 0.7)  %>%  withSpinner()
                                             ,collapsible = T,solidHeader = T,width = NULL,status = "info",title="Currencies"))
                                #Disable flickering during Refresh
                                ,tags$style(type="text/css", "#plotEagle_index.recalculating { opacity: 1.0; }")
                                ,tags$style(type="text/css", "#plotEagle_commodity.recalculating { opacity: 1.0; }")
                                ,tags$style(type="text/css", "#plotEagle_currency.recalculating { opacity: 1.0; }")
                                )
                            )
                    )
             )
    )
}


fnUI_myShares <- function(){
    
    fluidPage(
        fluidRow(column(width = 12,uiOutput("valueBoxes_myShares"))
                 ,fluidRow(
                     # column(width=2
                     #        ,box(
                     #            hr()
                     #            ,dateInput("dtmyShares_start",label = "Start Date",value = "2019-01-01",format = "dd-M-yy",autoclose = F)
                     #             ,dateInput("dtmyShares_end",label = "End Date",format = "dd-M-yy")
                     #             ,hr()
                     #             ,checkboxGroupInput('chkbmyShares_movAvgs',label = "Moving averages",choices = c("50-day MA","100-day MA","200-day MA"),inline = F)
                     #             ,br()
                     #             ,width = "100%"
                     #             ,status = "info"
                     #             ,solidHeader = T
                     #             ,title = "Parameter Selection")
                     # )
                     column(width = 12
                             ,box(plotOutput("plotmyShares",height = PLOT_HEIGHT*1.5)  %>%  withSpinner()
                                  ,collapsible = T,solidHeader = T,width = NULL,status = "info",title ="My Shares")
                             #Disable flickering during Refresh
                             ,tags$style(type="text/css", "#plotmyShares.recalculating { opacity: 1.0; }")
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
                               ,uiOutput("lovDeep_all")))
                  ,column(width = 7
                         ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Year to date trends"
                              ,plotOutput("plotDeep_YTDFcst",height = PLOT_HEIGHT)))
                  ,column(width = 3
                          ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Real Time"
                               ,plotOutput("plotDeep_realTime",height = PLOT_HEIGHT)))
        )
        # #Disable flickering during Refresh
        # ,tags$style(type="text/css", "#plotDeep_YTDFcst.recalculating { opacity: 1.0; }")
        # ,tags$style(type="text/css", "#plotDeep_realTime.recalculating { opacity: 1.0; }")
        # 

    )

}



















