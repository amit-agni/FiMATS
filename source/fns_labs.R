#UI and Server function definitions for the Labs page

###############     Called from ui.R      ################
fnUI_labs <- function(){
    
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
                               #,strong(textOutput('sectorCorrelations_text1'))
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


###############     Called from server.R      ################
fnServer_labs <- function(input,output,session){
    
    ######################     1. Correlations     #######################
    
    # output$sectorCorrelations_text1 <- renderText({
    #     paste("Period :",strftime(input$dt_start,format = '%d%b%y')," - ",strftime(input$dt_end,format = '%d%b%y'),sep="")
    # })
    
    
    output$sectorCorrelations_text2 <- renderText({
        x <- fn_corTopBottom(correl())
        paste('For example : The daily price returns of '
              ,x[order(-value)][1]$var1
              ,' and '
              ,x[order(-value)][1]$var2
              ,' shows a high correlation of '
              ,round(x[order(-value)][1]$value,2)
              ,' for the selected period : '
              ,strftime(input$dt_start,format = '%d%b%y')," - ",strftime(input$dt_end,format = '%d%b%y')
              ,sep="")
    })
    
    correl <- reactive({
        fn_opp_SectorCorrelations(DT_hist()[country == 'Australia' & category == 'stock'
                                            & date >= input$dt_start & date <= input$dt_end])
    })
    
    output$sectorCorrelations_plot <- renderPlot({
        ggcorrplot::ggcorrplot(correl(), hc.order = F, type = "lower",lab=TRUE,lab_size = 4)
        #,title = "Log of Daily Returns - Sector Correlations")
        # +
        #   theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),  # Order: top, right, bottom, left
        #         axis.text.y = element_text(margin=margin(0,-2,0,0)))
        # 
        # https://stackoverflow.com/questions/41269593/customize-ggcorrplot
        # ggplot(melt(correl()), aes(Var1, Var2, fill=value)) +
        #   geom_tile(height=0.8, width=0.8) +
        #   scale_fill_gradient2(low="blue", mid="white", high="red") +
        #   theme_minimal() +
        #   coord_equal() +
        #   labs(x="",y="",fill="Corr") +
        #   theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1,
        #                                  margin=margin(-3,0,0,0)),
        #         axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
        #         panel.grid.major=element_blank())
        
    })
    
    
    output$sectorCorrelations_table <- render_tableHTML({
        
        x <- fn_corTopBottom(correl())
        
        x[,value:=round(value,2)]
        
        setnames(x,c('var1','var2','value'),c('Sector 1','Sector 2','Correlation Coefficient'))
        rbind(x[order(-`Correlation Coefficient`)][1:10]
              ,x[order(`Correlation Coefficient`)][1:10][order(-`Correlation Coefficient`)]
              ,fill=T) %>% 
            tableHTML(row_groups = list(c(10, 10), c('Top Ten', 'Bottom Ten'))
                      ,widths = c(100,180,180,80)
                      ,rownames = F
                      ,spacing = '4px') %>%
            add_theme('rshiny-blue')
    })
    
    
    
    
    ######################### 2. Risk Rewards #########################
    
    output$riskReward_sectorLov <- renderUI({
        selectInput("riskReward_sectorLov",label = "Choose Sector",choices = c('ALL',unique(DT_stats()[category == 'stock']$sector)))
    })
    
    
    
    output$riskReward_excludeStocksLov <- renderUI({
        selectInput("riskReward_excludeStocksLov",label = "Exclude Stocks"
                    ,choices = unique(DT_stats()[category == 'stock']$name)
                    ,multiple = T)
    })
    
    
    DT_riskReward <- reactive({ 
        req(input$riskReward_sectorLov )
        if(input$riskReward_sectorLov == 'ALL'){
            fn_opp_RiskReward(DT_hist()[country == 'Australia' & category == 'stock' 
                                        & !name %in% input$riskReward_excludeStocksLov
                                        & date >= input$dt_start & date <= input$dt_end])  
        }else{
            fn_opp_RiskReward(DT_hist()[country == 'Australia' & category == 'stock' 
                                        & !name %in% input$riskReward_excludeStocksLov
                                        & sector == input$riskReward_sectorLov
                                        & date >= input$dt_start & date <= input$dt_end])  
        }
        
    })
    
    output$riskReward_plot <- renderPlot({
        DT_riskReward() %>% 
            ggplot(aes(x=risk,y=reward)) +
            geom_point(aes(color=risk,size=reward)) +
            facet_wrap(~sector,ncol=3) +
            geom_text_repel(aes(label=name),size=4) + #,scales="free") 
            theme(strip.text.x = element_text(size = 12)
                  ,axis.title = element_text(size = 16)
                  ,axis.text = element_text(size = 16)) +
            scale_color_gradient(low = "green", high = "red", na.value = NA)
        
    })
    
    output$riskReward_table <- render_tableHTML({
        
        DT_riskReward() %>% 
            tableHTML(rownames = F
                      ,widths = c(80,180,180,80,80)) %>%
            add_theme('rshiny-blue')
        
        # row_groups = list(c(10, 10), c('Top Ten', 'Bottom Ten'))
        #             ,widths = c(100,180,180,80)
        #             ,rownames = F
        #             ,spacing = '4px') %>%
        #   add_theme('rshiny-blue')
        
    })
    
    ######################### 3.  Monte Carlo   #########################
    output$monteCarlo_stockLOV <- renderUI({
        selectInput("monteCarlo_stockLOV",label = "Choose Stock",choices = DT_stats()[category == 'stock']$name)
    })
    
    
    DT_monteCarlo <- reactive({
        req(input$monteCarlo_stockLOV)
        temp <- DT_hist()[name==input$monteCarlo_stockLOV,.(category,symbol,date,close,sector,country,name)]
        n <- 1L
        temp[,`:=`(close_log = log(close)
                   ,close_shift = shift(close,n=n,type = "lag")
                   ,close_shiftlog = shift(log(close),n=n,type = "lag")
                   ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
             ,by = symbol]
        # temp[,`:=`(close_log = (close)
        #            ,close_shift = shift(close,n=n,type = "lag")
        #            ,close_shiftlog = shift((close),n=n,type = "lag")
        #            ,close_DiffLagLog = (close-shift((close),n=n,type = "lag"))/shift((close),n=n,type = "lag")
        # ),by = symbol]
        
        temp
    })
    
    observe({
        mean <- mean(DT_monteCarlo()$close_DiffLagLog,na.rm = T)
        sd <- sd(DT_monteCarlo()$close_DiffLagLog,na.rm = T)
        price <- DT_monteCarlo()[order(-date)][1]$close
        
        days <- input$monteCarlo_sliderForecastPeriod
        simulations <- input$monteCarlo_sliderSimulations
        
        
        if(input$monteCarlo_type =='Random Walk'){
            matrix <- matrix(NA,nrow=days,ncol=simulations)
            for(col in 1:ncol(matrix)){
                matrix[1,col]<-price
                for(row in 2:nrow(matrix)){
                    matrix[row,col] <- fn_randomWalk(matrix[row-1,col],mean,sd)
                }
            }
            
        }else{
            matrix <- matrix(NA,nrow=days,ncol=simulations)
            for(col in 1:ncol(matrix)){
                matrix[1,col]<-price
                for(row in 2:nrow(matrix)){
                    matrix[row,col] <- fn_brownianMotion(matrix[row-1,col],row,mean,sd)
                }
            }
            
        }
        
        DT <- fn_mungeMatrix(matrix)
        
        output$monteCarlo_expectedPrice <- renderText({
            paste("Current Stock price:",round(price,1)
                  ,". Simulated Stock price at the end of "
                  ,days,"days is:",round(quantile(DT[session == max(DT$session)][,-1],probs = 0.5),1)
                  ,"(Confidence probability : 50%)")
            
        })
        output$monteCarlo_simulationsPlot <- renderPlot({
            fn_plotSimulations(DT)  +
                labs(title = paste("Movement of stock prices for",days,"days over",simulations,"Monte Carlo simulations")
                     ,ylab ="Stock price")
        },height=PLOT_HEIGHT)
        
        output$monteCarlo_historgram  <- renderPlot({
            DT[session == max(DT$session)] %>%
                melt(id.var="session") %>%
                ggplot(aes(x=value)) +
                geom_histogram(bins = 15) +
                cutlery::theme_darklightmix(color_theme = "lightcyan",legend_position = "bottom") +
                theme(strip.text.x = element_text(size = 14, colour = "black")
                      ,plot.background = element_rect(fill = 'white')
                      ,legend.position = "none") +
                labs(title = paste("Distribution of the simulated stock price after",max(DT$session),"days")
                     ,subtitle = paste("Current Stock price:",round(price,1))
                     ,xlab ="Stock price")
        },height=PLOT_HEIGHT)
        
        output$monteCarlo_probabilities <- render_tableHTML({
            quantile(DT[session == max(DT$session)][,-1]
                     ,probs = c(0,0.05,0.25,0.5,0.75,0.95,1))  %>% 
                tableHTML(rownames = F
                          #,caption = "Probabilities"
                          ,round=1)
        })
        
        
    })
    
}