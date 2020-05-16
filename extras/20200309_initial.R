if(!require(pacman)) { install.packages("pacman"); library(pacman)}
p_load(here,data.table,tidyverse,tictoc)
rm(list = ls())
#remove scientific notation in printing 
options(scipen=999) 
#Revert back : options(scipen=0) 
source(here::here("210_src_R-scripts-functions","functions","misc_functions.R"))
source(here::here("210_src_R-scripts-functions","functions","db_functions.R"))
#source('//virginblue.internal/departments$/Business Services/Business Insights/Sales/Contract-Reports/R_functions/db_functions.R')
if(!require(cutlery)) { devtools::install_github("amit-agni/cutlery"); library(cutlery)}


DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
DT_stats = readRDS(file=here::here("100_data_raw-input","DT_stats.Rds"))
# DT_realTime =readRDS(file=here::here("100_data_raw-input","DT_realTime.Rds")
# DT_myShares=readRDS(file=here::here("100_data_raw-input","DT_myShares.Rds")

str(DT_hist)

unique(DT_hist$name)

DT_yahoocodes <- fread(file=here::here("100_data_raw-input","2020-04-06 yahoo symbols.csv"))
unique(DT_yahoocodes$category)


tq_get_options() 
tq_index_options() 

#Average of past N days
fn_avg <- function(vec,days){
    mean(tail(vec,days))
}
# DT[,.(avg7=tail(.SD,7)[,mean(close)],tail(.SD,100)[,mean(close)]),.(symbol)]
# DT[,.(avg7=fn_avg(.SD$close,7)),.(symbol)]
days <- c(1,7,14,28,100)
DT[,lapply(days, function(x) {fn_avg(.SD$close,x)}),.(symbol)]

tail(DT[symbol =='CSL.AX'],365)[order(close)]


CURRENT_DATE <- Sys.Date()
CURRENT_DATE - 365

#52week high-lows
#DT[,.(low52wk=tail(.SD,365)[,min(close)],high52wk=tail(.SD,365)[,max(close)]),.(symbol)]
LOW_HIGH <- 365
DATE_LOOKBACK <- Sys.Date() - 365
DT_Summary <- DT[,c(tail(.SD,1)[,.(close,date)]
                    ,.SD[date >= DATE_LOOKBACK][which.min(close),.(low=close,lowvolume=volume,lowdate=date
                                                           ,lowsince=difftime(Sys.Date(),date,units="days"))]
                    ,.SD[date >= DATE_LOOKBACK][which.max(close),.(high=close,highvolume=volume,highdate=date
                                                           ,highsince=difftime(Sys.Date(),date,units="days"))])
                 ,symbol]

ASX100 <- ASX100[DT_Summary,on=c(yahoo_sym = "symbol")]


#Get additional fin data
#https://stackoverflow.com/questions/53980350/how-to-scrape-key-statistics-from-yahoo-finance-with-r

yahooQF()

p_load(quantmod)
what_metrics <- yahooQF(c("Symbol","Name","Name (Long)","Market","Market State","Exchange","Exchange Full Name"
  ,"50-day Moving Average"
  , "Change From 50-day Moving Average"
  , "Percent Change From 50-day Moving Average"
  , "200-day Moving Average"
  , "Change From 200-day Moving Average"
  , "Percent Change From 200-day Moving Average"
  , "Market Capitalization"
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
  , "Price Hint"))

getQuote('TCS.BO', what=what_metrics,row.names = F)

Symbols<-ASX100$yahoo_sym
finStats <- getQuote(Symbols, what=what_metrics,row.names = F)

setDT(finStats,keep.rownames = "yahoo_sym")


tq_index_options()
tq_index()


ASX100 <- ASX100[finStats,on="yahoo_sym"]

# TO DO
# Get the average PE Ratio for the sector


write.csv(ASX100[order(-`Market Cap`)],file=here::here("300_output","20200312_summary_out.csv"),row.names = F)




DT %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line() +
    facet_wrap(~symbol,scales = 'free_y') +
    theme_classic() +
    labs(x = 'Date',
         y = "Adjusted Price",
         title = "Price Chart") +
    scale_x_date(date_breaks = "month",
                 date_labels = "%b\n%y")



getSymbols("CBA.AX", from = '2017-01-01',warnings = FALSE,
           auto.assign = TRUE)
tail(CBA.AX)    
chart_Series(CBA.AX)



### 25Mar20
# Refresh data
DT_today <- tidyquant::tq_get("^AXJO",from = "2020-03-20")
setDT(DT_today)
DT_today[order(-date)]

yahooQF()
what_metrics <- yahooQF(c("Last Trade (Price Only)"
                          ,"Open"
                          ,"Days High"
                          ,"Days Low"
                          ,"Volume" ))

                          
DT_today2 <- getQuote("^AXJO", what=what_metrics,row.names = F)
setDT(DT_today2)
DT_today2


### catchup

DT_catchup <- tidyquant::tq_get("^BSESN",from =min(DT_hist[,.(date=max(date)),symbol]$date), get = "stock.prices")
setDT(DT_catchup)

DT_hist[symbol == '^BSESN',1:8]
DT_catchup[symbol == '^BSESN']

batchtools::ajoin(DT_catchup[symbol == '^BSESN']
                  ,DT_hist[symbol == '^BSESN',1:8])


### my shares
DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
DT_myShares=readRDS(file=here::here("100_data_raw-input","DT_myShares.Rds"))
DT_hist <- merge(DT_hist,DT_myShares
                 ,all.x=T
                 ,by.x=c("symbol","date")
                 ,by.y=c("symbol","transaction_date"))

DT_hist[symbol=='ALL.AX'] %>%
ggplot(aes(x=date,y=close)) +
  #geom_smooth(se=F) +
  #geom_point() +
  geom_line() +
  geom_point(data=DT_hist[symbol=='ALL.AX' & !is.na(price)],aes(y=price,color=transaction_type)) +
  geom_label(data=DT_hist[symbol=='ALL.AX' & !is.na(price)],aes(y=price,label=price),nudge_x = 360) +
  facet_wrap(~type+name,scales = "free",ncol=3) +
    cutlery::theme_darklightmix(color_theme = "light") +
  theme(strip.text.x = element_text(size = 12, colour = "black"))



### value boxes
DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
DT_hist[type=='index',.SD[which.max(date)],symbol]
        
DT_hist[type=='index' & !is.na(close),lapply(.SD[order(-date)],function(x) head(x,2)),symbol][
  ,.(date,close,prev_close=shift(close,n=1L,type="lead")
     ,var=paste(round(100*((close/shift(close,n=1L,type="lead"))-1),2),"%",sep="")),symbol][!is.na(prev_close)][1,3]




### Things tried for data loading and making it reactive
#rv <- eventReactive(input$ab_loadFreshData,fnSrv_loadFreshData(input,output,session))
#DT_stats() <- rv$DT_stats


DT_stats = readRDS(file=here::here("100_data_raw-input","DT_stats.Rds"))
temp <- DT_stats[,.N,.(category,name)][,-"N"]

temp

lapply(temp,list)

split(temp,by="category",keep.by = F)

split(DT_stats[,.N,.(category,name)][,-"N"],by="category") %>% print()


choices <- list("Stocks" = DT_stats[category == 'stock']$name
                ,"Indices" = DT_stats[category == 'index']$name
                ,"Currencies" = DT_stats[category == 'currency']$name
                ,"Commodities" = DT_statscategory == 'commodity']$name)

### candelstick
p_load(plotly)
str(DT_hist)

fig <- DT_hist[symbol=='ALL.AX'] %>% 
  plot_ly(x = ~date, type="candlestick"
          ,open =~open, close = ~close ,high = ~high, low = ~low) 

fig %>% layout(title = "Candlestick Chart",
               xaxis = list(rangeslider = list(visible = T)),
               yaxis = list(autorange = TRUE)
               )



### fillup missing dates
DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))
DT_stats = readRDS(file=here::here("100_data_raw-input","DT_stats.Rds"))


names(DT_hist)
min(DT_hist$date)
max(DT_hist$date)

DT_hist[category=='index',max(date),symbol]

tidyquant::tq_get('^FTSE',from = '2020-04-01', get = "stock.prices")
tidyquant::tq_get('^AXJO',from = '2020-04-01', get = "stock.prices")

tq_get_options()

quantmod::getSymbols()

DT_catchup[category=='index',date,symbol]

temp[category=='index',date,symbol]

DT_hist()[category=='index',date,symbol]

DT_catchup[symbol =='^BSESN']
DT_hist()[symbol =='^BSESN']

DT_catchup[symbol =='^AXJO']
DT_hist()[symbol =='^AXJO']


sym <- c("^BSESN","^AXJO","^VIX")
batchtools::ajoin(DT_catchup[symbol %in% sym]
                  ,DT_hist()[symbol  %in% sym & !is.na(close)][, .N, .(symbol, name, sector, country, category,date)][,-"N"])


## Indian stock codes

test <- tidyquant::tq_get('532540.BO',from = '2020-04-01', get = "stock.prices")




#!4may20 .. extra code i think remove it


# ######################
# #   deep analysis ###
# ######################
# 
# output$lovDeep_all <- renderUI({
#   req(DT_stats()$category)
#   selectInput("lovDeep_all",label = "What to analyse"
#               ,choices = list("Stocks" = DT_stats()[category == 'stock']$name
#                               ,"Indices" = DT_stats()[category == 'index']$name
#                               ,"Currencies" = DT_stats()[category == 'currency']$name
#                               ,"Commodities" = DT_stats()[category == 'commodity']$name)
#               ,selected = DT_stats()[1, ]$name)
# })
# 
# observe({
#   
#   req(input$lovDeep_all)
#   req(input$radioDeep_PlotYTDType)
#   
#   req(DT_hist())
#   req(DT_stats())
#   req(DT_myShares())
#   req(DT_realTime())
# 
#   output$tblDeep_KPI <- renderTable({fn_tblKPI(DT_hist(), DT_stats(), input$lovDeep_all)})
#   
#   
#   output$boxDeep_PlotYTD <- renderUI({
#   
#     box(collapsible = T,solidHeader = T,width = NULL,status = "success",title = "Year to date trends"
#         ,#height = paste(PLOT_HEIGHT,"px",sep="")
#         height = PLOT_HEIGHT+64
#         ,if(input$radioDeep_PlotYTDType =='Standard'){
#           renderPlotly({fn_plotYTD(DT_hist = DT_hist(),dt_start = input$dt_start,dt_end = input$dt_end,varSymbols = input$lovDeep_all
#                        ,DT_myShares = DT_myShares()
#             )})
#       }else{
#         renderPlotly({fn_plotFinance(DT_hist = DT_hist(),dt_start = input$dt_start,dt_end = input$dt_end,varSymbols = input$lovDeep_all
#                                      ,plotType = input$radioDeep_PlotYTDType
#           )})
#       }
#       
#       )
#     })
#   
#   #renderUI(renderPlot) gives object of type 'closure' is not subsettable
#   #renderUI(plotOutput) gives Text to be written must be a length-one character vector
#   
#   
#   output$plotDeep_realTime <- renderPlot({
#     if (input$radio_realTimeYN == "Yes") {
#         fn_plotRealTime(DT_realTime = DT_realTime(),varSymbols = input$lovDeep_all,DT_myShares = NULL)
#       }
#     },height = PLOT_HEIGHT)
#   
# }) 
# 

# 
# fnUI_deepAnalyse <- function(){
#   
#   fluidPage(
#     fluidRow(column(width = 12
#                     ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="KPIs"
#                          ,tableOutput("tblDeep_KPI"))))
#     ,fluidRow(column(width = 2
#                      ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Additional Parameters"
#                           ,uiOutput("lovDeep_all")
#                           ,radioButtons('radioDeep_PlotYTDType',label = 'Plot Type',choices = c("Standard","CandleStick","OHLC","Waterfall","Funnel","FunnelArea","Indicator"))))
#               ,column(width = 7,uiOutput("boxDeep_PlotYTD"))
#               ,column(width = 3
#                       ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Real Time"
#                            ,plotOutput("plotDeep_realTime",height = PLOT_HEIGHT)))
#     )
#     #Disable flickering during Refresh
#     ,tags$style(type="text/css", "#boxDeep_PlotYTD.recalculating { opacity: 1.0; }")
#     ,tags$style(type="text/css", "#plotDeep_realTime.recalculating { opacity: 1.0; }")
#     
#     
#   )
#   
# }

what_metrics <- yahooQF(c("Symbol"
                          ,"Last Trade (Price Only)"
                          ,"Open"
                          # ,"Days High"
                          # ,"Days Low"
                          # ,"Volume" 
))

for(i in unique(DT_stats[country == 'India']$symbol)){
  print(i)
  print(getQuote(i, what=what_metrics,row.names = F)    )
}

