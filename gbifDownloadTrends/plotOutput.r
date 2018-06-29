# plot output

plotOutput = function(input,ordersData) {
source("processInput.r")

# HT = processInput(input,ordersData) 
D = ordersData
D = D[D$order %in% input,]

HT = tidyr::spread(D, order, value) # table for highcharts
y = lubridate::year(HT$yearMonth)
m = month.abb[lubridate::month(HT$yearMonth)]
HT$my = paste(m,y) # month year

library(dplyr)
library(stringr)
library(purrr)
library(highcharter)

# highcharts_demo()
hc <- highchart() %>% 
hc_xAxis(categories = HT$my)

print(length(input))

for(i in 1:length(input)) {
hc <- hc %>% hc_add_series(name = input[i], data = HT[ ,input])
}

return(hc)
}












