<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![contributions
welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

<!-- badges: end -->

FiMATS
======

##### Financial Markets Analysis and Tracking System

R Shiny dashboard that can be used for tracking the financial markets.
The system makes use of the the `Yahoo! Finance` stock codes and the
`tidyquant` and `quantmod` packages for getting the stock prices.

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
-   Code Structure
    -   **ui.R** : Shiny file for generating the UI. The UI elements for
        rendering different pages is modularised and the respective
        functions are stored in separate .R files in the source folder
    -   **server.R** : Shiny file containing the server logic. Similar
        to the UI elements, the server elements are also stored in
        separate .R files
    -   **global.R** : Quoted from the documentation
        `Objects defined in global.R are similar to those defined in app.R outside of the server function definition, with one important difference: they are also visible to the code in the ui object. This is because they are loaded into the global environment of the R session; all R code in a Shiny app is run in the global environment or a child of it.`
    -   **data folder** : Contains all the data, images and HTML files
        that are needed in the app
    -   **source folder** : Contains the functions that will called in
        the app
