
fnUI_dataRefresh <- function(){
    
    verticalLayout(
        box(collapsible = T,solidHeader = T,status = "primary",title ="Initial Data Loading"
            ,fileInput("file_yahooCodes", "CSV File with symbols as in Yahoo"
                                   ,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                   ,width = '100%')
                        # ,checkboxGroupInput('chkb_loadWhat'
                        #                     ,label = "For the symbols in the selected file, get"
                        #                     ,choices = c("Daily historical values","Financial Stats")
                        #                     ,inline = F)
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
                 column(width=2,uiOutput("boxEagle_additionalParameters"))
                 ,column(width = 10,uiOutput("box_plotEagle")
                            ,fluidRow(
                                column(width = 6,uiOutput("box_plotEaglecommodity"))
                                ,column(width = 6,uiOutput("box_plotEaglecurrency"))
                                # #Disable flickering during Refresh
                                # ,tags$style(type="text/css", "#plotEagle_index.recalculating { opacity: 1.0; }")
                                # ,tags$style(type="text/css", "#plotEagle_commodity.recalculating { opacity: 1.0; }")
                                # ,tags$style(type="text/css", "#plotEagle_currency.recalculating { opacity: 1.0; }")
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
                     column(width = 12,uiOutput("box_mySharesPlot")
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



















