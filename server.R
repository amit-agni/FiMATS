server <- function(input, output, session) {
  
  #Server logic for Market Overview page
  fnServer_overview(input,output,session)  
  
  #Server logic for My Investments page
  fnServer_myInvestments(input,output,session)
  
  #Server logic for Technical Analysis page
  fnServer_technicalAnalysis(input,output,session)
  
  #Server logic for Labs page
  fnServer_labs(input,output,session)
  
  #Server logic for Data Refresh page
  fnServer_dataRefresh(input,output,session)
  

}








