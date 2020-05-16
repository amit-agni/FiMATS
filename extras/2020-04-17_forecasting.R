if(!require(pacman)) { install.packages("pacman"); library(pacman)}
p_load(here,data.table,tidyverse,tictoc)
p_load(tsibble)
p_load(fable)
p_load(feasts)
p_load(imputeTS)
p_load(ggcorrplot)
p_load(DescTools)
p_load(purrr)
p_load(PerformanceAnalytics)
p_load(quantmod)
p_load(ggrepel)

rm(list = ls())
options(scipen=999) 
if(!require(cutlery)) { devtools::install_github("amit-agni/cutlery"); library(cutlery)}

DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))


## Charting
xts(DT_hist[symbol == '^AXJO'][,.(open,high,low,close,volume)],order.by =DT_hist[symbol == '^AXJO']$date)  %>%
    chartSeries(TA='addBBands();addRSI();addVo();addMACD()',subset='2020')

xts(DT_hist[symbol == '^AXJO'][,.(open,high,low,close,volume)],order.by =DT_hist[symbol == '^AXJO']$date)  %>%
    chart_Series(TA='addMACD();addVo();addMACD()',subset='2020')

png('out.png')
myPars <-chart_pars() 
myPars$cex<-2
xts(DT_hist[symbol == '^AXJO'][,.(open,high,low,close,volume)],order.by =DT_hist[symbol == '^AXJO']$date)  %>%
    chart_Series(type="")


addRSI()

#;addSMA(n=30,col="white");addRSI();addSMI();addWPR()

#addADX Add Directional Movement Index
#addBBands Add Bollinger Bands to Chart
#addCCI Add Commodity Channel Index
#addExpiry Add Contract Expiration Bars to Chart
#addMA Add Moving Average to Chart
    #addSMA() addEMA() addWMA() addDEMA() addEVWMA() addZLEMA()
#addMACD Add Moving Average Convergence Divergence to Chart
#addROC Add Rate Of Change to Chart
#addRSI Add Relative Strength Index to Chart
#addSAR Add Parabolic Stop and Reversal to Chart
#addSMI Add Stochastic Momentum Indicator to Chart
#addVo Add Volume to Chart
#addWPR Add William’s Percent R to Chart


#20Apr
# add bolinger
# add RSI
# Average Directional Index (ADX) looks good but needs more investigation
# SMA
# MACD https://school.stockcharts.com/doku.php?id=technical_indicators:moving_average_convergence_divergence_macd







temp <- DT_hist[,.(symbol,date,close)]
temp <- as_tsibble(temp,key=symbol,index=date)
temp <- temp %>% fill_gaps(.full = T) #fill gaps with NA
temp <- temp %>% group_by(symbol) %>%
    mutate(close=imputeTS::na_interpolation(close)) #Interpolate the NAs

#Exploratory plots
temp %>% filter(symbol == '^BSESN') %>% group_by_key() %>% gg_season()
temp %>% filter(symbol == '^BSESN') %>% autoplot(close)

#Forecast plots
sym <- c("^BSESN","^AXJO","^VIX")
temp %>% filter(symbol %in% sym) %>% group_by_key() %>%
    model(
        ets = ETS(box_cox(close,1)),
        arima = ARIMA(close,stepwise = F,greedy = F),
        snaive = SNAIVE(close)
    ) %>%
    forecast(h = "1 month") %>%
    autoplot(filter(temp,symbol %in% sym & year(date) >= 2020 & month(date)>2),level = NULL)


#Stock Analysis
#https://towardsdatascience.com/analyzing-stocks-using-r-550be7f5f20d
DT_hist = readRDS(file=here::here("100_data_raw-input","DT_hist.Rds"))

#Difference of log of lag differences
temp <- DT_hist[,.(category,symbol,date,close,sector,country)]
n <- 1L
temp[,`:=`(close_log = log(close)
           ,close_shift = shift(close,n=n,type = "lag")
           ,close_shiftlog = shift(log(close),n=n,type = "lag")
           ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
     ,by = symbol]


#correlation of indices
correl <- temp[category == 'index'] %>%
    dcast(date~symbol,value.var ="close_DiffLagLog") %>%
    .[,-"date"] %>%
    cor(.,use = "complete.obs")

ggcorrplot::ggcorrplot(correl, hc.order = FALSE, type = "lower",lab=TRUE,lab_size = 2)

#correlation of stock
correl <- temp[category == 'stock'] %>%
    dcast(date~symbol,value.var ="close_DiffLagLog") %>%
    .[,-"date"] %>%
    cor(.,use = "complete.obs")

ggcorrplot::ggcorrplot(correl, hc.order = F, type = "lower",lab=TRUE,lab_size = 2) +
    theme(axis.text.x = element_text(size = 4)
          ,axis.text.y = element_text(size = 4))

#correlation of sector
correl <- temp[category =='stock'
               ,.(close_DiffLagLog=mean(close_DiffLagLog,na.rm = T)),.(sector,date)] %>%
    dcast(date~sector,value.var ="close_DiffLagLog") %>%
    .[,-"date"] %>%
    cor(.,use = "complete.obs")
ggcorrplot::ggcorrplot(correl, hc.order = F, type = "lower",lab=TRUE,lab_size = 2) 
x <- fn_corTopBottom(correl)
x[order(value)]

#Yearly change in correlation
fn_cors <- function(data,names){
    correl <- cor(data,use = "complete.obs") %>% fn_corTopBottom()
    correl$name <- names
    correl
}

fn_corTopBottom <- function(correl){
    correl[upper.tri(correl,diag = TRUE)] <- NA
    correl <- setDT(as.data.frame(correl),keep.rownames = TRUE)
    melt(correl,id.vars = "rn",variable.factor = F)[order(-value)] %>% setnames(.,c("rn","variable"),c("var1","var2"))
}

x <- temp[category =='stock' & country =='Australia'
          ,.(close_DiffLagLog=mean(close_DiffLagLog,na.rm = T)),.(sector,date)] %>%
    dcast(date~sector,value.var ="close_DiffLagLog") 

out <- map2(.x=split(x[,-1],year(x$date))
     ,.y=unique(year(x$date))
     ,.f = fn_cors) 

out <- do.call("rbind",out)

out[name != '2020'] %>% ggplot(aes(x=name,y=value)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0.8,color="darkgreen") +
    geom_hline(yintercept = 0.5,color="blue") +
    geom_hline(yintercept = 0.2,color="red") +
    facet_wrap(~paste(var1,var2,sep="\n")) +
    geom_rect(data = out[name %in%  c('2017','2018','2019') & value <0.2 ]
              ,fill = "red"
              ,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.3)  +
    geom_rect(data = out[name %in%  c('2017','2018','2019') & value > 0.65 & value < 1]
              ,fill = "green"
              ,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.3) +
    labs(title = "Yearly Correlation of log of lag differences"
         ,subtitle = "Until 2019, And for 2017-2019 : Green boxes indicate > 0.65 and Red boxes indicate < 0.2") + 
    theme_darklightmix() +
    theme(axis.text.x = element_text(size = 5)
          ,axis.text.y = element_text(size = 5)
          ,strip.text = element_text(size = 6
                                     , margin = ggplot2::margin(0, 0, 0.4, 0, unit = "cm")))

    
    




# Risk-Reward
#I first compared the risk/return rate of each stock. I took the mean of log return and standard deviation of log return.
#The mean is assumed as the consistent rate of return while standard deviation is the risk that comes with purchasing 
#the stock. I used plotly, an interactive visualization tool, to illustrate my findings.

temp <- DT_hist[,.(category,symbol,date,close,sector,country,name)]
n <- 1L
temp[,`:=`(close_log = log(close)
           ,close_shift = shift(close,n=n,type = "lag")
           ,close_shiftlog = shift(log(close),n=n,type = "lag")
           ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
     ,by = symbol]


#mean and sd of the log return
temp[year(date) == '2020' & category == 'stock',.(reward = mean(close_DiffLagLog,na.rm = T)
                           ,risk = sd(close_DiffLagLog,na.rm = T))
     ,.(symbol,name,sector,year(date))] %>%
    ggplot(aes(x=reward,y=risk)) +
    geom_point() +
    facet_wrap(~sector) +
    geom_text_repel(aes(label=name),size=3) #,scales="free")



# RANDOM WALK AND MONTE CARLO 

# I generated the prices using the data I have earlier from log returns and used exponential growth rate to predict how much the 
# stock will grow per day. The growth rate is randomly generated and dependent on the input values of mu and sigma.
temp <- DT_hist[,.(category,symbol,date,close,sector,country,name)]
n <- 1L
temp[,`:=`(close_log = log(close)
           ,close_shift = shift(close,n=n,type = "lag")
           ,close_shiftlog = shift(log(close),n=n,type = "lag")
           ,close_DiffLagLog = log(close)-shift(log(close),n=n,type = "lag"))
     ,by = symbol]


# One Simulation / RANDOM WALK
sim <- temp[symbol =='^AXJO' & year(date) == '2019']
mu <- mean(sim$close_DiffLagLog,na.rm = T)  # mean of log returns
sig <- sd(sim$close_DiffLagLog,na.rm = T) # sd of log returns 
most_recent_price <- max(sim$close)
testsim<-rep(NA,1000)

price[1]<-most_recent_price #most recent price
price<-rep(NA,252*4)#one year 252 trading days, simulate for 4 years 

#start simulating prices
for(i in 2:length(testsim)){
    price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data %>% 
    ggplot(aes(Day,Price))+
    geom_line()+
    labs(title="Amazon (AMZN) price simulation for 4 years")+
    theme_bw()


#Multiple Sims / MONTE CARLO
N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-most_recent_price
for(j in 1:ncol(mc_matrix)){
    mc_matrix[1,j]<-most_recent_price
    for(i in 2:nrow(mc_matrix)){
        mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
    }
}
name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)
final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name
dim(final_mat) #1008 501
final_mat%>%gather("Simulation","Price",2:501)%>%
    ggplot(aes(x=Day,y=Price,Group=Simulation))+
    geom_line(color="blue",alpha=0.2)+
    labs(title="Amazon Stock (AMZN): 500 Monte Carlo Simulations for 4 Years")+
    theme_bw()

#Amazon (AMZN)’s stock may reach the price of $11198.10 in four years time or crash to a $834.60 low (the 0% value).
most_recent_price
final_mat[500,-1]%>%as.numeric()%>%quantile(probs = c(seq(0.1,1,0.1)))




# Geometric Brownian motion
#https://blog.alookanalytics.com/2017/04/26/monte-carlo-method-in-r/


#' Stock price calculation
#' 
#' Calculates stock price after n periods using standard stock price model
#' @param stock_price original stock price
#' @param n number of periods
#' @param stock_mu expected percentual stock drift over n periods
#' @param stock_sigma expecter percentual stock volatility
#' @return stock price after n periods
f_stock_return <- function(stock_price, n, stock_mu, stock_sigma){
    delta_t <- 1/n # one period
    for (i in seq(n)){
        epsilon <- runif(n=1, min=0, max=1) # random generated number
        # calculate stock price (using quantile function of normal distribution)
        stock_price <- stock_price * (1 + qnorm(epsilon, 
                                                stock_mu * delta_t, 
                                                stock_sigma* sqrt(delta_t)))
    }
    return(stock_price)
}

# parameters
simulations <- 500 # number of MC simulations
n <- 20 # trading days
stock_price <- most_recent_price
stock_mu <- mu # drift 10%
stock_sigma <- sig # volatility 20%

# Monte Carlo simulations
set.seed(42) # for reproducibility
stock_prices <- c()
for (i in seq(simulations)){
    stock_prices <- c(stock_prices,
                      f_stock_return(stock_price=stock_price, 
                                     n=n, 
                                     stock_mu=stock_mu, 
                                     stock_sigma=stock_sigma))
}

stock_prices
random_data<-cbind(stock_prices,1:n)
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data %>% 
    ggplot(aes(Day,Price))+
    geom_line()+
    labs(title="Amazon (AMZN) price simulation for 4 years")+
    theme_bw()




## Forecasting
# https://www.quantstart.com/articles/topic/time-series-analysis/
# ARima : https://www.quantstart.com/articles/Autoregressive-Integrated-Moving-Average-ARIMA-p-d-q-Models-for-Time-Series-Analysis/
# Garch : https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis/
# Arima + Garch : https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R/



## SMA trading stategy
# Good explanation : https://ntguardian.wordpress.com/2017/03/27/introduction-stock-market-data-r-1/



#Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

fnUI_charts <- function(){
    
    fluidPage(fluidRow(box(width = 12,solidHeader = T,background = "navy"
                           ,column(width = 6,h4("Technical Charts and Forecasts"))
                           ,column(width = 6,uiOutput("lovCharts_all"))))
              ,fluidRow(column(width = 2
                               ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Additional Parameters"
                                    ,uiOutput("lovDeep_all")
                                    ,radioButtons('radioDeep_PlotYTDType',label = 'Plot Type',choices = c("Standard","CandleStick","OHLC","Waterfall","Funnel","FunnelArea","Indicator"))))
                        ,column(width = 7,uiOutput("boxDeep_PlotYTD"))
                        ,column(width = 3
                                ,box(collapsible = T,solidHeader = T,width = NULL,status = "success",title ="Real Time"
                                     ,plotOutput("plotDeep_realTime",height = PLOT_HEIGHT)))
              )
    )
    
    
}




,tabItem(tabName = "menu_charts",fnUI_charts())