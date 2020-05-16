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


temp <- DT_hist[,.(category,symbol,date,close,sector,country,name)]
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


temp <- temp[symbol == '^AXJO' & year(date) == '2019']

mean <- mean(temp$close_DiffLagLog,na.rm = T)
sd <- sd(temp$close_DiffLagLog,na.rm = T)
price <- temp[order(-date)][1]$close

days <- 250
simulations <- 1000L
matrix <- matrix(NA,nrow=days,ncol=simulations)

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
        theme(legend.position = "none") 
}


#Random Walk - Normal distribution
fn_randomWalk <- function(price,mean,sd){
    price * exp(rnorm(1,mean,sd))
}

for(col in 1:ncol(matrix)){
    matrix[1,col]<-price
    for(row in 2:nrow(matrix)){
        matrix[row,col] <- fn_randomWalk(matrix[row-1,col],mean,sd)
    }
}

DT <- fn_mungeMatrix(matrix)
fn_plotSimulations(DT)

quantile(DT[session == max(DT$session)][,-1])
hist(t(DT[session == max(DT$session)][,-1]))
DT[session == max(DT$session)] %>%
    melt(id.var="session") %>%
    ggplot(aes(x=value)) +
    geom_histogram(bins = 15)



#Brownian motion
fn_brownianMotion <- function(price, n, mean, sd){
    delta_t <- 1/n # one period
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    # calculate stock price (using quantile function of normal distribution)
    price * (1 + qnorm(epsilon, mean * delta_t, sd * sqrt(delta_t)))
    
}

for(col in 1:ncol(matrix)){
    matrix[1,col]<-price
    for(row in 2:nrow(matrix)){
        matrix[row,col] <- fn_brownianMotion(matrix[row-1,col],row,mean,sd)
    }
}

DT <- fn_mungeMatrix(matrix)
head(DT)
fn_plotSimulations(DT)

quantile(DT[session == max(DT$session)][,-1])



###testing brownian
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


simulations <- 100 # number of MC simulations
n <- 250 # trading days
stock_price <- price
stock_mu <- mean # drift 10%
stock_sigma <- sd # volatility 20%

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

quantile(stock_prices)




### Implement these forumule
# https://uu.diva-portal.org/smash/get/diva2:1218088/FULLTEXT01.pdf
# https://ro.uow.edu.au/cgi/viewcontent.cgi?article=1705&context=aabfj
# https://quant.stackexchange.com/questions/43781/how-to-calculate-mean-and-volatility-parameters-for-geometric-brownian-motion

