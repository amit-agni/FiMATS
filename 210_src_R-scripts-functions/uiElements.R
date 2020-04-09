
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
                 ,column(width = 10,uiOutput("box_plotEagle")
                         ,tags$style(type="text/css", "#box_plotEagle.recalculating { opacity: 1.0; }")
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
                               ,uiOutput("lovDeep_all")))
                  ,column(width = 7
                         ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Year to date trends"
                              ,plotOutput("plotDeep_YTDFcst",height = PLOT_HEIGHT)))
                  ,column(width = 3
                          ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Real Time"
                               ,plotOutput("plotDeep_realTime",height = PLOT_HEIGHT)))
        )
        # #Disable flickering during Refresh
         ,tags$style(type="text/css", "#plotDeep_YTDFcst.recalculating { opacity: 1.0; }")
         ,tags$style(type="text/css", "#plotDeep_realTime.recalculating { opacity: 1.0; }")
         

    )

}



















