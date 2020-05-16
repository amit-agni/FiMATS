buttonWidth <- 200
sideBarWidth <- 300

shinydashboard::dashboardPage(
    skin = "purple"
    #Header
    ,header = dashboardHeader(
      title = "FiMATS"
      ,titleWidth = sideBarWidth
      ,dropdownMenu(type="notifications"
                    ,headerText = "TO DO"
                    ,icon = icon("info")
                    ,badgeStatus = NULL
                    ,messageItem(from="Work in Progress"
                                 ,message="App for tracking. multilines not working.How to use this app"
                                 ,icon = icon("chart-line"))
                    ,notificationItem("Market Overview",icon = icon("globe-americas"))
                    ,notificationItem("Data Processing",icon = icon("download")))
      ,tags$li(
        a("github",
          height = 40,
          href = "https://github.com/amit-agni/FiMATS",
          title = "",
          #target = "_blank"
          ),
        class = "dropdown"
        )
      )
    
    #Sidebar
    ,sidebar = dashboardSidebar(width = sideBarWidth
                                ,sidebarMenu(id="menuTabs"
                                             ,br()
                                             #,fluidPage(tags$i("Financial Markets Analysis and Tracking System"))
                                             ,fluidPage(div(style='text-align:center;'
                                                            ,span("Financial Markets Analysis and Tracking System"
                                                                  ,style="color:yellow;font-size:12px")
                                                            ,br()
                                                            ,span(textOutput('text_LatestDate')
                                                                 ,style="color:white;font-size:10px;")
                                                            ,span("(Best viewed on a Desktop PC or Mobile Landscape mode)"
                                                                  ,style="color:white;font-size:10px;font-style:italic")
                                             )

                                             )
                                             
                                             ,br()
                                             ,menuItem("Markets Overview",tabName = 'menu_overview',icon = icon("globe-americas"),startExpanded = F)
                                             ,menuItem("My Investments",tabName = 'menu_myInvestments',icon = icon("list-alt"),startExpanded = F)
                                             ,menuItem("Technical Analysis",tabName = "menu_technicalAnalysis",icon = icon("chart-line"),startExpanded = F)
                                             ,menuItem("Labs",tabName = 'menu_labs',icon = icon("unlock"),startExpanded = F)
                                             ,menuItem("Data Processing",tabName = "menu_dataRefresh",icon = icon("database"),startExpanded = F)
                                             ,menuItem("Common Parameters",icon=icon("sliders-h"),startExpanded = T
                                             # ,column(width = 12,strong("Enable Realtime")
                                             # ,shinyWidgets::switchInput('radio_realTimeYN'
                                             #                            #,label="Enable Realtime"
                                             #                            ,value = F,onLabel = "Yes",offLabel = "No"
                                             #                            ,size = "large",inline = T)
                                             # )
                                             # ,shinyWidgets::materialSwitch('radio_realTimeYN2'
                                             #                               ,label="Enable Realtime"
                                             #                               ,value = F
                                             #                               ,status = "primary"
                                             #                               ,inline = F)
                                             ,dateInput("dt_start",label = "Start Date",value = Sys.Date()-60,format = "dd-M-yy")
                                             ,dateInput("dt_end",label = "End Date",format = "dd-M-yy")
                                             #,uiOutput("sideBardynamicParams")
                                             ,radioButtons('radio_realTimeYN',label="Enable Realtime"
                                                           ,choices = c("Yes","No"),selected = "No",inline = T)
                                             ,sliderInput("slider_realTimeDelay","Realtime Delay (in secs)",min=5,max=60,value=10,ticks = F,step = 5)
                                             ,hr()
                                             #,checkboxGroupInput('chkb_movAvgs',label = "Moving averages",choices = c("50-day MA","100-day MA","200-day MA"),inline = F)
                                             #,br()
                                             )
                                             )
                                )
    #Body
    ,body = dashboardBody(
      
      tabItems(
        tabItem(tabName = "menu_overview",fnUI_overview())
        ,tabItem(tabName = "menu_myInvestments",fnUI_myInvestments())
        ,tabItem(tabName = "menu_technicalAnalysis",fnUI_technicalAnalysis())
        ,tabItem(tabName = "menu_labs",fnUI_labs())
        ,tabItem(tabName = "menu_dataRefresh",fnUI_dataRefresh())
        

      )
      #value box height
      ,tags$head(tags$style(HTML(".small-box {height: 80px}")))
      
      )
    )

