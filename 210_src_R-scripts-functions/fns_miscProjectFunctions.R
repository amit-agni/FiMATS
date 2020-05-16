####    Functions used in the project     ####

#######################################################
################    1. Data Load     ##################
#######################################################
what_metrics <- quantmod::yahooQF(c("Name (Long)"
                                    ,"Market Capitalization"
                                    , "Price/Sales"
                                    , "P/E Ratio"
                                    , "PEG Ratio"
                                    , "Price/EPS Estimate Next Year"
                                    , "Price/Book"
                                    , "Book Value"
                                    , "Shares Outstanding"
                                    , "Ex-Dividend Date"
                                    , "Dividend/Share"
                                    , "Dividend Yield"
                                    , "Earnings/Share"
                                    , "Price Hint"
                                    , "52-week High"
                                    , "Percent Change From 52-week High"
                                    , "52-week Low"
                                    , "Percent Change From 52-week Low"
                                    , "50-day Moving Average"
                                    , "Percent Change From 50-day Moving Average"
                                    , "200-day Moving Average"
                                    , "Percent Change From 200-day Moving Average"
                                    , "Last Trade (Price Only)"
                                    , "Last Trade Time"
))

fn_getData_DTStats <- function(DT_yahooCodes){
    #For those symbols, get the key stats. what_metrics is defined in global.R
    DT_stats_temp <- getQuote(DT_yahooCodes$symbol, what=what_metrics,row.names = F)
    setDT(DT_stats_temp,keep.rownames = "symbol")
    setnames(DT_stats_temp
             ,names(DT_stats_temp)
             ,tolower(str_replace_all(str_replace_all(names(DT_stats_temp),"[[:punct:]]","")," ","_")))
    
    #output$txt_dataStatus <- renderText({ "Financial Stats loaded"})
    
    DT_stats_temp <- merge(DT_stats_temp,DT_yahooCodes,all.x =T,by = "symbol")
    DT_stats_temp[is.na(name),name:=namelong] #If name is not provided in the CSV file then use the yahoo name
    DT_stats_temp[,namelong:=NULL]
    
    var_lvls <- DT_stats_temp[,sum(market_capitalization,na.rm = T),name][order(-V1)]$name
    DT_stats_temp[,name := factor(name,levels = var_lvls,ordered = T)]
    
    DT_stats_temp
    
}


fn_getData_DThist <- function(DT_yahooCodes,DT_stats,input){
    DT_hist_temp <- tidyquant::tq_get(DT_yahooCodes$symbol
                                      ,from = input$date_dataStartDate
                                      #,to = Sys.Date()
                                      , get = "stock.prices")
    setDT(DT_hist_temp)
    
    #This is needed as the realtime comparison happens on prior date
    DT_hist_temp <- DT_hist_temp[date < Sys.Date()] 
    
    # #Fill missing dates
    # min <- min(DT_hist_temp$date)
    # max <- max(DT_hist_temp$date)
    # date_index <- DT_hist_temp[,.(date = seq(min,max,by="day")),by=symbol]
    # DT_hist_temp <- merge(date_index,DT_hist_temp,by=c("symbol","date"),all.x = T)
    
    DT_hist_temp <- merge(DT_hist_temp,DT_stats[,.(symbol,name,category,sector,country)]
                          ,all.x =T,by = "symbol") #Merge only the CSV file fields in historical
    
    DT_hist_temp
}


fn_getData_DTrealTime <- function(DT_stats){
    
    what_metrics <- yahooQF(c("Symbol"
                              ,"Last Trade (Price Only)"
                              ,"Open"
                              # ,"Days High"
                              # ,"Days Low"
                              # ,"Volume" 
    ))
    
    temp <- try(getQuote(unique(DT_stats$symbol), what=what_metrics,row.names = F))
    
    if(class(temp) != "data.frame"){
        temp <- data.frame()
        print(temp)
        print("error in real time loading")
    }else{
        setDT(temp)
        setnames(temp,"Symbol","symbol")
        temp <- merge(temp,DT_stats,all=F,by="symbol")
    }
    
    # TODO : Handle error in symbols. Removed Error in M&M.BO. Below code takes long time
    # temp_list <- lapply(unique(DT_stats$symbol), function(i){
    # tryCatch(expr = getQuote(i, what=what_metrics,row.names = F)
    #                      ,error = function(e) {
    #                          #print(paste("Error in one of the symbol"))
    #                          #print(e)
    #                          return(-1)})
    # })
    # temp <- rbindlist(temp_list)
    # temp <- temp[!is.na(Symbol)]    
    
    print("completed realtime")
    temp
    
}

#################################################
################    2. Plots     ################
#################################################

fn_plotYTD <- function(DT_hist,dt_start,dt_end,varSymbols,DT_myShares,displayPerPage=NULL){
    
    #browser()
    
    if(is.null(varSymbols)){ #Eagle Eye
        if(!is.null(DT_myShares)){
            temp <- DT_hist[symbol %in% unique(DT_myShares$symbol) & date >= dt_start & date <= dt_end]
        }else{
            temp <- DT_hist[date >= dt_start & date <= dt_end]
        }
        
    }else{ #Deep Analysis
        temp <- DT_hist[name %in% varSymbols & date >= dt_start & date <= dt_end]
    }
    
    #Used for adjusting the date axis scale, scale_x_date
    var_countDates <- as.numeric(difftime(dt_end,dt_start,units ="days"))
    
    
    if(!is.null(displayPerPage)){
        if(displayPerPage != 'ALL'){
            temp <- temp[name %in% unique(temp$name)[1:displayPerPage]]    
        }
    }
    
    var_plot <- temp %>%
        ggplot(aes(x=date,y=close)) +
        geom_line() +
        facet_wrap(~name,scales = "free_y",ncol=4) +
        scale_x_date(date_breaks = fnHelper_dateBreaks(var_countDates)
                     ,date_labels = fnHelper_dateLabels(var_countDates)
        ) +
        cutlery::theme_darklightmix(color_theme = "lightcyan",legend_position = "bottom") +
        theme(strip.text.x = element_text(size = 14, colour = "black")
              ,plot.background = element_rect(fill = 'white'))
    
    if(!is.null(DT_myShares)){ #Eagle Eye and Deep Analysis
        temp <- merge(temp,DT_myShares
                      ,all.x=T
                      ,by.x=c("symbol","date")
                      ,by.y=c("symbol","transaction_date"))
        
        var_plot <- var_plot +
            geom_point(data=temp[!is.na(price)],aes(y=price,color=transaction_type),size=5) +
            ggrepel::geom_text_repel(data=temp[!is.na(price)]
                                     ,aes(y=price,label=paste(price,strftime(date,format="%d%b%y"),sep="\n"))
                                     ,direction = "x"
                                     #,min.segment.length = 0 #,min.segment.length = unit(0, 'lines')
                                     ,nudge_y = 0.5
            ) +
            scale_color_manual(values = c("#00AFBB", "#FC4E07"))  # green for buy and red for sell
        # scale_shape_manual(values = c("Buy"=2,"Sell"=6)) 
    }
    var_plot   
}



fn_plotFinance <- function(DT_hist,dt_start,dt_end,varSymbols,plotType){
    DT_hist[name==varSymbols & date >= dt_start & date <= dt_end] %>% 
        plot_ly(x = ~date, type=tolower(plotType)
                ,open =~open, close = ~close ,high = ~high, low = ~low) %>%
        layout(title = paste(plotType,"Chart")
               ,xaxis = list(rangeslider = list(visible = T))
               ,yaxis = list(autorange = TRUE))    
    
}


fn_plotRealTime <- function(DT_realTime,varSymbols,DT_myShares,displayPerPage=NULL){
    
    #DT_realTime <- merge(DT_realTime,DT_stats,all=F,by="symbol")
    if(is.null(varSymbols)){ #
        if(!is.null(DT_myShares)){
            temp <- DT_realTime[symbol %in% unique(DT_myShares$symbol)]
        }else{
            temp <- DT_realTime
        }
    }else{
        temp <- DT_realTime[name %in% varSymbols ]
    }
    
    if(!is.null(displayPerPage)){
        if(displayPerPage != 'ALL'){
            temp <- temp[name %in% unique(temp$name)[1:displayPerPage]]    
        }
    }
    
    
    temp %>%  
        ggplot(aes(x=`Trade Time`,y=Last)) +
        geom_line() +
        facet_wrap(~name,scales = "free",ncol=4) +
        cutlery::theme_darklightmix(color_theme = "va_light") +
        theme(strip.text.x = element_text(size = 14, colour = "black")
              ,plot.background = element_rect(fill = 'white'))
}



##################################################
################    2. Tables     ################
##################################################


fn_tblKPI <- function(DT_hist,DT_stats,varSymbols){
    
    # temp1 <- DT_stats[name %in% varSymbols][
    #     ,.(trade_time
    #        ,last = round(last,0)
    #        ,`52high` = paste(round(`52week_high`,0)," (",round(`_change_from_52week_high`*100,2),"%)",sep="")
    #        ,`52low` = paste(round(`52week_low`,0)," (",round(`_change_from_52week_low`*100,2),"%)",sep="")
    #        ,`50day` = paste(round(`50day_ma`,0)," (",round(`_change_from_50day_ma`*100,2),"%)",sep="")
    #        ,`200day` = paste(round(`200day_ma`,0)," (",round(`_change_from_200day_ma`*100,2),"%)",sep="")
    #        )
    #     ]
    # 
    
    latest <- DT_hist[name %in% varSymbols][order(-date)][1]$close
    
    temp1 <- DT_stats[name %in% varSymbols
                      ,.(`52high` = paste(round(`52week_high`,0)," (",round((`52week_high`/latest - 1 )*100,2),"%)",sep="")
                         ,`52low` = paste(round(`52week_low`,0)," (",round((`52week_low`/latest - 1 )*100,2),"%)",sep="")
                         ,`50day MA` = paste(round(`50day_ma`,0)," (",round((`50day_ma`/latest - 1 )* 100,2),"%)",sep="")
                         ,`200day MA` = paste(round(`200day_ma`,0)," (",round((`200day_ma`/latest - 1 ) * 100,2),"%)",sep="")
                      )
                      # ,.(`52high` = round(`52week_high`,0)
                      #    ,`52low` = round(`52week_low`,0)
                      #    ,`50day` = round(`50day_ma`,0)
                      #    ,`200day` = round(`200day_ma`,0))
                      ]
    
    
    temp2 <- DT_hist[name %in% varSymbols][order(-date)][
        ,.(date,close,varPct = round(100 * (close/shift(close,n=1L,type="lead")-1),2))][1:6][
            ,.(day=strftime(date,format = "%d %b-%a")
               ,close = paste(round(close,0)," (",varPct,"%)",sep=""))] %>%
        melt(id.vars="day") %>%
        dcast(variable ~ factor(day, levels=day)) %>%
        .[,2:7] 
    cbind(temp2,temp1)
    
} 



fn_tblWinnersLosers <- function(DT_hist,DT_realTime){
    
    temp <- merge(DT_hist[,.(name,date,close=round(close,1))]
                  ,DT_realTime[,.(name,last=round(Last,1))]
                  ,by="name")
    
    temp[,growth := round(100*(last/close-1),1)]
    
    temp[order(growth)]
}



################################################
################    4. Labs     ################
################################################

################    4.1 Correlations     ################

fn_corTopBottom <- function(correl){
    correl[upper.tri(correl,diag = TRUE)] <- NA
    correl <- setDT(as.data.frame(correl),keep.rownames = TRUE)
    out <- melt(correl,id.vars = "rn",variable.factor = F)[order(-value)] %>% setnames(.,c("rn","variable"),c("var1","var2"))
    out
}


fn_opp_SectorCorrelations <- function(DT_hist){
    
    temp <- DT_hist[,.(close=mean(close,na.rm = T)),.(date,sector)]
    
    #Difference of log of lag differences
    n <- 1L
    temp[,`:=`(close_log = log(close)
               ,close_shift = shift(close,n=n,type = "lag")
               ,close_shiftlog = shift(log(close),n=n,type = "lag")
               ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))]
    
    correl <- temp %>%
        dcast(date~sector,value.var ="close_DiffLagLog") %>%
        .[,-"date"] %>%
        cor(.,use = "complete.obs")
    
    correl
}


################    4.2 Risk Reward     ################

fn_opp_RiskReward <- function(DT_hist){
    
    temp <- DT_hist[,.(category,symbol,date,close,sector,country,name)]
    n <- 1L
    temp[,`:=`(close_log = log(close)
               ,close_shift = shift(close,n=n,type = "lag")
               ,close_shiftlog = shift(log(close),n=n,type = "lag")
               ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
         ,by = symbol]
    
    #mean and sd of the log return
    temp <- temp[,.(reward = round(mean(close_DiffLagLog,na.rm = T),4)
                    ,risk = round(sd(close_DiffLagLog,na.rm = T),4))
                 ,.(symbol,name,sector)]
    
    temp
    
    
    
    
    
}

################    4.3 Monte Carlo     ################

#Common functions
fn_mungeMatrix <- function(matrix){
    DT <- data.table(matrix)
    setnames(DT,names(DT),paste('sim',1:ncol(DT),sep="_"))
    DT$session <- seq(1:nrow(DT))
    setcolorder(DT,"session")
    DT
}

fn_plotSimulations <- function(DT){
    DT %>% 
        melt(id.vars = "session") %>%
        ggplot(aes(x=session,y=value,color=variable)) +
        geom_line(alpha = 0.3) +
        cutlery::theme_darklightmix(color_theme = "lightcyan",legend_position = "bottom") +
        theme(strip.text.x = element_text(size = 14, colour = "black")
              ,plot.background = element_rect(fill = 'white')
              ,legend.position = "none")
}


#Random Walk - Normal distribution
fn_randomWalk <- function(price,mean,sd){
    price * exp(rnorm(1,mean,sd))
}

#Brownian motion
fn_brownianMotion <- function(price, n, mean, sd){
    delta_t <- 1/n # one period
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    # calculate stock price (using quantile function of normal distribution)
    price * (1 + qnorm(epsilon, mean * delta_t, sd * sqrt(delta_t)))
    
}




###################################################
################    n. Helpers     ################
###################################################

fnHelper_shinyBusy <- function(value,text="in progress",session=NULL){
    if(value==T){
        shinybusy::show_modal_spinner(text = text, spin = "atom", color ="tomato") # show the modal window
    }else{
        remove_modal_spinner() # remove it when done
        #show sucess message
        sendSweetAlert(
            session,
            title = "Success",
            #text = "done",
            type = "success",
            btn_labels = "Ok",
            btn_colors = "#3085d6",
            html = FALSE,
            closeOnClickOutside = TRUE,
            showCloseButton = FALSE,
            width = NULL
        )
        
    }
    
}


fnHelper_success <- function(session){
    sendSweetAlert(
        session,
        title = "Success",
        #text = "done",
        type = "success",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
    )
    
}

fnHelper_kable <- function(DT){
    DT %>%
        kable(format.args = list(decimal.mark = '.', big.mark = ",")) %>%
        kable_styling(bootstrap_options = "condensed"
                      ,full_width = FALSE
                      ,position = "left"
                      ,font_size = 10)
}


fnHelper_dateBreaks <- function(x){
    case_when(x < 30 ~ "day"
              ,x >=30 & x < 120 ~ "week"
              ,x >=120 & x < 365*1.5 ~ "month"
              ,TRUE ~ "3 months"
    )
}


fnHelper_dateLabels <- function(x){
    case_when(x < 30 ~ "%d\n%b\n%a"
              ,x >=30 & x < 120 ~ "%d\n%b"
              ,x >=120 & x < 365*1.5 ~ "%b\n%y"
              ,TRUE ~ "%b\n%y"
    )
}

gg_facet_nrow <- function(p){
    assertive.types::assert_is_any_of(p, 'ggplot')
    p %>%
        ggplot2::ggplot_build() %>%
        magrittr::extract2('layout') %>% 
        magrittr::extract2('layout') %>%
        magrittr::extract2('ROW') %>%
        unique() %>%
        length()
}

