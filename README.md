FiMATS is a R Shiny dashboard that can be used for tracking the
financial markets. The system makes use of the the `Yahoo! Finance`
stock codes and the `tidyquant` and `quantmod` packages for getting the
stock prices.

-   Following features are currently implemented :
    -   Loading of `Yahoo! Finance` stock codes using a CSV file. The
        file should have these column names :
        country,category,sector,symbol,name
    -   The stocks owned by the user can be added using a CSV file with
        these mandatory fields :
        symbol,transaction\_date,transaction\_type,price,qty
    -   Live realtime stock prices
    -   Technical analysis for the chosen stock
    -   Experimental Features
        -   The Australian stocks are placed on Risk Reward chart
        -   Correlation of the log returns for different sectors
        -   Monte Carlo Simulations
-   In the pipeline
    -   Cloud integration (AWS/Dropbox)
    -   Stock price forecasting
    -   Identifying opportunities to invest
