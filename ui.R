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
                                             ,menuItem("Markets Overview",tabName = 'menu_eagleEye',icon = icon("globe-americas"),startExpanded = F)
                                             ,menuItem("My Investments",tabName = 'menu_myShares',icon = icon("list-alt"),startExpanded = F)
                                             #,menuItem("Deep Dive",tabName = "menu_deepDive",icon = icon("swimmer"),startExpanded = F)
                                             ,menuItem("Technical Analysis",tabName = "menu_charts",icon = icon("chart-line"),startExpanded = F)
                                             ,menuItem("Labs",tabName = 'menu_opportunities',icon = icon("unlock"),startExpanded = F)
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
                                             ,radioButtons('radio_realTimeYN',label="Enable Realtime"
                                                           ,choices = c("Yes","No"),selected = "No",inline = T)
                                             ,sliderInput("slider_realTimeDelay","Realtime Delay (in secs)",min=2,max=60,value=6,ticks = F,step = 2)
                                             ,hr()
                                             #,checkboxGroupInput('chkb_movAvgs',label = "Moving averages",choices = c("50-day MA","100-day MA","200-day MA"),inline = F)
                                             #,br()
                                             )
                                             )
                                )
    #Body
    ,body = dashboardBody(
      
      tabItems(
        tabItem(tabName = "menu_eagleEye",fnUI_eagleEye())
        ,tabItem(tabName = "menu_myShares",fnUI_myShares())
        #,tabItem(tabName = "menu_deepDive",fnUI_deepAnalyse())
        ,tabItem(tabName = "menu_charts",fnUI_charts())
        ,tabItem(tabName = "menu_opportunities",fnUI_opportunities())
        ,tabItem(tabName = "menu_dataRefresh",fnUI_dataRefresh())
        

      )
      #value box height
      ,tags$head(tags$style(HTML(".small-box {height: 80px}")))
      
      )
    )

