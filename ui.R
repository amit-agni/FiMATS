buttonWidth <- 200
sideBarWidth <- 300

shinydashboard::dashboardPage(
    skin = "purple"
    #Header
    ,header = dashboardHeader(
      title = "FiMATS"
      ,titleWidth = sideBarWidth
      ,dropdownMenu(type="notifications"
                    ,headerText = "Information"
                    ,icon = icon("info")
                    ,badgeStatus = NULL
                    ,messageItem(from="Work in Progress"
                                 ,message="App for tracking. multilines not working.How to use this app"
                                 ,icon = icon("chart-line"))
                    ,notificationItem("World Markets",icon = icon("globe-americas"))
                    ,notificationItem("Data Processing",icon = icon("download")))
      ,tags$li(
        a("github",
          height = 40,
          href = "https://github.com/amit-agni",
          title = "",
          target = "_blank"
          ),
        class = "dropdown"
        )
      )
    
    #Sidebar
    ,sidebar = dashboardSidebar(width = sideBarWidth
                                ,sidebarMenu(id="menuTabs"
                                             ,br()
                                             ,fluidPage(tags$i("Financial Markets Analysis and Tracking System"))
                                             ,br()
                                             ,menuItem("World Markets",tabName = 'menu_eagleEye',icon = icon("globe-americas"),startExpanded = F)
                                             ,menuItem("My Investments",tabName = 'menu_myShares',icon = icon("sticky-note"),startExpanded = F)
                                             ,menuItem("Deep Dive",tabName = "menu_deepDive",icon = icon("swimmer"),startExpanded = F)
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
                                             ,dateInput("dt_start",label = "Start Date",value = "2019-01-01",format = "dd-M-yy")
                                             ,dateInput("dt_end",label = "End Date",format = "dd-M-yy")
                                             ,radioButtons('radio_realTimeYN',label="Show Realtime"
                                                           ,choices = c("Yes","No"),selected = "No",inline = T)
                                             ,sliderInput("slider_realTimeDelay","Realtime Delay (in secs)",min=2,max=60,value=6,ticks = F,step = 2)
                                             ,hr()
                                             ,checkboxGroupInput('chkb_movAvgs',label = "Moving averages",choices = c("50-day MA","100-day MA","200-day MA"),inline = F)
                                             ,br()
                                             )
                                             )
                                )
    #Body
    ,body = dashboardBody(
      
      tabItems(
        tabItem(tabName = "menu_eagleEye",fnUI_eagleEye())
        ,tabItem(tabName = "menu_myShares",fnUI_myShares())
        ,tabItem(tabName = "menu_deepDive",fnUI_deepAnalyse())
        ,tabItem(tabName = "menu_dataRefresh",fnUI_dataRefresh())
        

      )
      #value box height
      ,tags$head(tags$style(HTML(".small-box {height: 80px}")))
      
      )
    )

