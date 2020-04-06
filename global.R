if(!require(pacman)) { install.packages("pacman"); library(pacman)}
p_load(here,data.table,tidyverse,tictoc,shiny,shinydashboard,shinybusy,kableExtra,quantmod,gridExtra,
       shinyBS
       ,shinyWidgets #switchinput
       ,shinyjs #hide show
       ,tableHTML #make_css
       ,shinycssloaders #spinner busy
       ,ggrepel
       ,tidyquant
       ,quantmod
       ,batchtools
       ,assertive.types #needed for gg_facet_nrow()
       )
rm(list = ls())
if(!require(cutlery)) { devtools::install_github("amit-agni/cutlery"); library(cutlery)}

source(here::here("210_src_R-scripts-functions","uiElements.R"))
source(here::here("210_src_R-scripts-functions","serverElements.R"))


DT_stats <- reactive(readRDS(file=here::here("100_data_raw-input","DT_stats.Rds")))
DT_hist <- reactive(readRDS(file=here::here("100_data_raw-input","DT_hist.Rds")))
DT_myShares <- reactive(readRDS(file=here::here("100_data_raw-input","DT_myShares.Rds")))
#DT_realTime <- reactive(readRDS(file=here::here("100_data_raw-input","DT_realTime.Rds")))



PLOT_HEIGHT <- 500
FACET_ROW_HEIGHT <- 250
NO_OF_VALUE_BOXES <- 12


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




###############################
##    Common functions     ####
###############################

fn_tblKPI <- function(DT_hist,DT_stats,varSymbols){
    temp1 <- DT_stats[name %in% varSymbols][
        ,.(last = round(last,0)
           ,`52high` = paste(round(`52week_high`,0)," (",round(`_change_from_52week_high`*100,2),"%)",sep="")
           ,`52low` = paste(round(`52week_low`,0)," (",round(`_change_from_52week_low`*100,2),"%)",sep="")
           ,`50day` = paste(round(`50day_ma`,0)," (",round(`_change_from_50day_ma`*100,2),"%)",sep="")
           ,`200day` = paste(round(`200day_ma`,0)," (",round(`_change_from_200day_ma`*100,2),"%)",sep=""))
        ]
    temp2 <- DT_hist[name %in% varSymbols][order(-date)][
        ,.(date,close,varPct = round(100 * (close/shift(close,n=1L,type="lead")-1),2))][1:6][
            ,.(day=strftime(date,format = "%d %b-%a")
               ,close = paste(close," (",varPct,"%)",sep=""))] %>%
        melt(id.vars="day") %>%
        dcast(variable ~ factor(day, levels=day)) %>%
        .[,2:7] 
    cbind(temp1,temp2)
} 



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


fnHelper_shinyBusy <- function(value,text="in progress"){
    if(value==T){
        shinybusy::show_modal_spinner(text = text, spin = "atom", color ="tomato") # show the modal window
    }else{
        remove_modal_spinner() # remove it when done
    }
    
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

