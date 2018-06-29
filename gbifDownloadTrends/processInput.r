# process input gbif trends 


# load("C:/Users/ftw712/Desktop/gbifTrends/highChartsTest/uniqueOrders.rda")
# load("C:/Users/ftw712/Desktop/gbifTrends/highChartsTest/ordersData.rda")

# input = uniqueOrders[1:3]
 
processInput = function(input,ordersData) {
D = ordersData
D = D[D$order %in% input,]

HT = tidyr::spread(D, order, value) # table for highcharts
y = lubridate::year(HT$yearMonth)
m = month.abb[lubridate::month(HT$yearMonth)]
HT$my = paste(m,y) # month year 

return(HT)
}


