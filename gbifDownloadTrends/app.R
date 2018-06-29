library("shiny")
library("highcharter")


load("uniqueChoices.rda")
# load("ordersData.rda")
# source("plotOutput.r")
load("allTaxaHT.rda")
HT = allTaxaHT
enableBookmarking("url")

ui = function(request) {

localEnv= "http://127.0.0.1:3029"
prodEnv = "https://johnwaller.shinyapps.io/gbifDownloadTrends"
queryStrings =c(
"/?_inputs_&selectInput=%5B%22Panthera%20leo%20total%3A%20279%22%2C%22Panthera%20tigris%20total%3A%20236%22%2C%22Ursidae%20total%3A%201685%22%5D",
"/?_inputs_&selectInput=%5B%22Arabis%20total%3A%20178%22%2C%22Drosophila%20total%3A%20105%22%2C%22Anolis%20total%3A%2038%22%5D",
"/?_inputs_&selectInput=%5B%22Aves%20total%3A%2048648%22%2C%22Mammalia%20total%3A%2050740%22%2C%22Reptilia%20total%3A%2021941%22%2C%22Insecta%20total%3A%2028892%22%5D",
"/?_inputs_&selectInput=%5B%22Hymenoptera%20total%3A%207005%22%2C%22Coleoptera%20total%3A%205912%22%2C%22Odonata%20total%3A%201270%22%2C%22Lepidoptera%20total%3A%207371%22%5D",
"/?_inputs_&selectInput=%5B%22Animalia%20total%3A%20208011%22%2C%22Plantae%20total%3A%20196763%22%2C%22Bacteria%20total%3A%20219%22%2C%22Fungi%20total%3A%205839%22%5D"
)

links = paste0(prodEnv,queryStrings)
# links = paste0(localEnv,queryStrings)
    
fluidPage(
titlePanel("gbif download trends"),
sidebarLayout(

sidebarPanel(
helpText("See how gbif occurrence downloads have trended over time based on the species or group downloaded."), 
helpText("If you can't find a species or group it might not have been downloaded or was downloaded less than 25 times."),
selectizeInput('selectInput', 'pick a species or group:', choices = uniqueChoices, multiple = TRUE),
helpText("Don't know where to start? Try one of our presets:"),
tags$ol(
  tags$li(tags$a(href=links[1], "lions, tigers, and bears?")), 
  tags$li(tags$a(href=links[2], "model organisms")), 
  tags$li(tags$a(href=links[3], "birds, mammals, reptiles, and insects")),
  tags$li(tags$a(href=links[4], "popular insect groups")),
  tags$li(tags$a(href=links[5], "the kingdoms"))
),
helpText("Total downloads are calculated by rolling up all downloads below a certain taxonomic level. 
         So for example, if a user downloaded 5 different bird species in a single download, 
         this would still count as 1 download for Aves. It would also count as 1 download for each of the species in  the original download.
         In other words, a user would not have to specifically entered the name a higher taxanomic group for it to be counted here.")
),
mainPanel(
highchartOutput("hcontainer",height = "500px")
)
)
)
}

server = function(input, output, session) {

  if(TRUE) { # set up bookmarking
    
    observe({
      reactiveValuesToList(input)
      session$doBookmark()
    })
    onBookmarked(function(url) {
      updateQueryString(url)
    })
    
  }
  
  
    
output$hcontainer <- renderHighchart({
  
  if(length(input$selectInput) >= 1) {
  
  library(dplyr)
  library(stringr)
  library(purrr)
  library(highcharter)
  
  export = list(list(text="jpeg image",onclick=JS("function () { this.exportChart({ type: 'image/jpeg' }); }")))
 
    
  hc <- highchart() %>% hc_xAxis(categories = HT$my) %>%  hc_yAxis(title = list(text = "total monthly downloads")) %>%
    hc_exporting(enabled=TRUE,formAttributes=list(target="_blank"),buttons=list(contextButton=list(theme=list(fill="transparent"),text="",menuItems=export)))
  
  
    
  x = input$selectInput
  for(i in 1:length(x)) {
    hc <- hc %>% hc_add_series(name = x[i], data = HT[ ,x[i]]) 
  }
  hc
  }
  
})
}

shinyApp(ui = ui, server = server)