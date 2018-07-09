library(shiny)
library(leaflet)
library(CoordinateCleaner)
library(countrycode)
library(plyr)




ui <- pageWithSidebar(
headerPanel('bird diversity in Brazil'),
sidebarPanel(
sliderInput("resolution", "resolution",50, 500,value = 100, step = 50,ticks=FALSE,post="km"),
sliderInput("occurrence", "min number occurrences",1,10000,value = 1, step = 1,ticks=FALSE),
textInput(inputId="textValue",value=1,label=NULL),
sliderInput("year","recorded after",1870,2010,value = 1, step = 10,ticks=FALSE,sep=""),
radioButtons("focalColumn", "count values",choices = c(species="species",genus="genus",family="family",occurrences="occurrences"),selected = "species"),
checkboxInput("centroidsCountry", "show potential country centroids", FALSE),
checkboxInput("centroidsProvince", "show potential province centroids", FALSE),
checkboxInput("centroidsCapitals", "show potential capital centroids", FALSE)
),
mainPanel(
leafletOutput("map", height = '570')
)

)


server <- function(input, output, session) {
load("roundedBirdCoord.rda")

observeEvent(input$textValue, {
x = input$textValue
if(x == "") { x = 1 }
if(x != input$occurrence) {
updateSliderInput(session=session,inputId='occurrence',value=input$textValue)
}
},ignoreNULL = TRUE)
observeEvent(input$occurrence,{
x = input$textValue
if(x == "") { x = 1 }
if(x != input$occurrence) {
updateTextInput(session=session,inputId='textValue',value=input$occurrence)
}
}, ignoreNULL = TRUE)  
  
    
filteredData <- reactive({ 
out = D[D$roundTo == (input$resolution/100) & D$occurrences >= input$occurrence,] 
out = out[out$year == input$year,]
# out = aggregate(cbind(species,genus,family,freq) ~ coordId + decimallatitudeRounded + decimallongitudeRounded + roundTo, data=D, sum)
return(out)
})

capitalsData <- reactive({ 
# capitals = CoordinateCleaner::capitals
load("capitals.rda")
capitals = capitals[capitals$ISO3 == "BRA",]
capitals$longitude = roundCoordinates(capitals$longitude,(input$resolution/100))
capitals$latitude = roundCoordinates(capitals$latitude,(input$resolution/100))
return(capitals)
})

centroidData <- reactive({ 
source("roundCoordinates.r")
  
# cc = CoordinateCleaner::centroids
load("centroids.rda")
cc = centroids
cc = cc[cc$iso3 == "BRA",]

out = cc # save to out just in case none are checked
if(input$centroidsCountry & !input$centroidsProvince) out = cc[cc$type == "country",]
if(!input$centroidsCountry & input$centroidsProvince) out = cc[cc$type == "province",]

out$longitude = roundCoordinates(out$longitude,(input$resolution/100))
out$latitude = roundCoordinates(out$latitude,(input$resolution/100))
return(out)
})
  
output$map <- renderLeaflet({
leaflet(D) %>% addTiles() %>%
fitBounds(~min(decimallongitudeRounded), ~min(decimallatitudeRounded), ~max(decimallongitudeRounded), ~max(decimallatitudeRounded))
})


observe({
plotData = filteredData()

focalColumn = input$focalColumn
plotData$fc = plotData[,focalColumn]

pal = colorBin("YlOrRd", domain = c(min(plotData$fc),max(plotData$fc)),bins = 5,pretty=FALSE,reverse=FALSE)
plotData$countLabel = as.character(plotData$fc)
plotData$color = pal(plotData$fc)



leafletProxy("map",data=plotData) %>%
clearShapes() %>%
addCircles(~decimallongitudeRounded,~decimallatitudeRounded, weight=1,radius=5e4*(input$resolution/100),fillColor=~color,color=~color,label=~countLabel,opacity=1,fillOpacity=1) %>%
clearControls() %>%
addLegend("bottomright",pal=pal,values=~fc,title=paste("number of",input$focalColumn),opacity=1)
})


observe({
cc = centroidData()

proxy <- leafletProxy('map',data=cc) %>% clearGroup(group="centroidsCountry")

if(input$centroidsCountry) {
proxy %>% addMarkers(~longitude, ~latitude,group="centroidsCountry",label=~name)
} 
if(!input$centroidsCountry) {
proxy %>% clearGroup(group="centroidsCountry")
}
})
observe({
cc = centroidData()

proxy <- leafletProxy('map',data=cc) %>% clearGroup(group="centroidsProvince")

if(input$centroidsProvince) {
proxy %>% addMarkers(~longitude, ~latitude,group="centroidsProvince",label=~name)
}
if(!input$centroidsProvince) {
proxy %>% clearGroup(group="centroidsProvince")
}
})
observe({
caps = capitalsData()

proxy <- leafletProxy('map',data=caps) %>% clearGroup(group="centroidsCapitals")

if(input$centroidsCapitals) {
proxy %>% addMarkers(~longitude, ~latitude,group="centroidsCapitals")
}
if(!input$centroidsCapitals) {
proxy %>% clearGroup(group="centroidsCapitals")
}
})



}

shinyApp(ui, server, options = list(height = 5000))

