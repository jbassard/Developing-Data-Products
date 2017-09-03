# Server calculations coding for a Shiny web application.
options(shiny.maxRequestSize=70*1024^2) # increase upload limit to 70Mo for the csv files to upload on the shiny server

# Loading necessary packages
if (!require("shiny")) {
  install.packages("shiny")}
library(shiny)

## For plotting 
if (!require("ggplot2")) {
  install.packages("ggplot2")}
if (!require("ggvis")) {
  install.packages("ggvis")}
if (!require("devtools")) {
  install.packages("devtools")}
if (!require("Rcpp")) {
  install.packages("Rcpp")}
if (!require("rCharts")) { 
  install_github('ramnathv/rCharts')}

library(Rcpp)
library(ggplot2)
library(devtools)
library(rCharts)
library(ggvis)

## For data processing
if (!require("data.table")) {
  install.packages("data.table")}
if (!require("reshape2")) {
  install.packages("reshape2")}
if (!require("dplyr")) {
  install.packages("dplyr")}
library(data.table)
library(reshape2)
library(dplyr)

## To generate Markdown files
if (!require("markdown")) {
  install.packages("markdown")}
library(markdown)

## For plotting maps on shinyapps.io
if (!require("mapproj")) {
  install.packages("mapproj")}
if (!require("maps")) {
  install.packages("maps")}
library(mapproj)
library(maps)


# Loading data
states_map <- map_data("state")
dt <- fread('./events.csv')
dt$EVTYPE <- tolower(dt$EVTYPE)
evtypes <<- sort(unique(dt$EVTYPE))

# Shiny server
shinyServer(function(input, output){

## Define and initialize reactive values
  values <- reactiveValues()
  values$evtypes <- evtypes  
  
## Create event type checkbox
  output$evtypeControls <- renderUI({
    checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
    
    })
  
## Add observers on clear and select all buttons
  observe({
    if(input$clear_all == 1) return()
    values$evtypes <- NULL
  })
  
  observe({
    if(input$select_all == 1) return()
    values$evtypes <- evtypes
  })  
  

## Prepare dataset
### Aggregate dataset by states
  dt.agg <- reactive({
    tmp <- merge(
      data.table(STATE=sort(unique(dt$STATE))),dt[YEAR >= input$range[1] & YEAR <= input$range[2] & EVTYPE %in% input$evtypes,
                                                  list(COUNT=sum(COUNT),INJURIES=sum(INJURIES),FATALITIES=sum(FATALITIES),PROPDMG.TOTAL=round(sum(PROPDMG.TOTAL), 2),CROPDMG.TOTAL=round(sum(CROPDMG.TOTAL), 2)),
                                                  by=list(STATE)],
      by=c('STATE'), all=TRUE
    )
    tmp[is.na(tmp)] <- 0
    tmp
  })
  
  ### Aggregate dataset for time series 
  dt.agg.year <- reactive({dt[YEAR >= input$range[1] & YEAR <= input$range[2] & EVTYPE %in% input$evtypes,
                              list(COUNT=sum(COUNT),INJURIES=sum(INJURIES),PROPDMG.TOTAL=round(sum(PROPDMG.TOTAL), 2),FATALITIES=sum(FATALITIES),CROPDMG.TOTAL=round(sum(CROPDMG.TOTAL), 2)),
                              by=list(YEAR)]
  })
  
  ### Prepare dataset for downloads  
  prepare_downolads <- function(dt) {
    dt %>% rename(
      State = STATE, Count = COUNT,
      Injuries = INJURIES, Fatalities = FATALITIES,
      Property.damage = PROPDMG.TOTAL, Crops.damage = CROPDMG.TOTAL
    ) %>% mutate(State=state.abb[match(State, tolower(state.name))])}
  dataTable <- reactive({
    prepare_downloads(dt.agg())
  })
  
## Render Plots
### Prepare map of economic or population impact by state
  
  output$populationImpactByState <- renderPlot(
    {
      data <- dt.agg()
      if(input$populationCategory == 'both') {
        data$Affected <- data$INJURIES + data$FATALITIES
      } else if(input$populationCategory == 'fatalities') {
        data$Affected <- data$FATALITIES
      } else {
        data$Affected <-data$INJURIES
      }
      
      title <- paste("Population impact", input$range[1], "-", input$range[2], "(Number of Affected People)")
      p <- ggplot(data, aes(map_id = STATE))
      p <- p + geom_map(aes(fill = Affected), map = states_map, colour='black')
      p <- p + expand_limits(x = states_map$long, y = states_map$lat)
      p <- p + coord_map() + theme_bw()
      p <- p + labs(x = "Long", y = "Lat", title = title)
       print(p)
    })
  
  output$economicImpactByState <- renderPlot({
    data <- dt.agg()
    
    if(input$economicCategory == 'both') {
      data$Damages <- data$PROPDMG.TOTAL + data$CROPDMG.TOTAL
    } else if(input$economicCategory == 'crops') {
      data$Damages <- data$CROPDMG.TOTAL
    } else {
      data$Damages <- data$PROPDMG.TOTAL
    }
    
    title <- paste("Economical impact", input$range[1], "-", input$range[2], "(Million USD)")
    p <- ggplot(data, aes(map_id = STATE))
    p <- p + geom_map(aes(fill = Damages), map = states_map, colour='black') + expand_limits(x = states_map$long, y = states_map$lat)
    p <- p + coord_map() + theme_bw()
    p <- p + labs(x = "Long", y = "Lat", title = title)
    print(p)
  })
  
  output$evtypeControls <- renderUI({
    if(1) {
      checkboxGroupInput('evtypes', 'Event types', evtypes, selected=evtypes)
    }
  })
  
  dataTable <- reactive(
    {
      dt.agg()[, list(State=state.abb[match(STATE, tolower(state.name))],
                      Count=COUNT,Injuries=INJURIES,Fatalities=FATALITIES,Property.damage=PROPDMG.TOTAL,Crops.damage=CROPDMG.TOTAL)]   
    })
  

### Number of Events by year  
  output$eventsByYear <- renderChart({
    data <- dt.agg.year()[, list(COUNT=sum(COUNT)), by=list(YEAR)]
    setnames(data, c('YEAR', 'COUNT'), c("Year", "Count"))
    
    eventsByYear <- nPlot(Count ~ Year,data = data[order(data$Year)],type = "lineChart", dom = 'eventsByYear', width = 650)
    
    eventsByYear$chart(margin = list(left = 100))
    eventsByYear$yAxis( axisLabel = "Count", width = 80)
    eventsByYear$xAxis( axisLabel = "Year", width = 70)
    return(eventsByYear)
  })
 
### Population impacted by year   
  output$populationImpact <- renderChart({
    data <- melt(dt.agg.year()[, list(Year=YEAR, Injuries=INJURIES, Fatalities=FATALITIES)],id='Year')
    populationImpact <- nPlot(value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
                              type = 'stackedAreaChart', dom = 'populationImpact', width = 650)
    
    populationImpact$chart(margin = list(left = 100))
    populationImpact$yAxis( axisLabel = "Number of Affected People", width = 80)
    populationImpact$xAxis( axisLabel = "Year", width = 70)
    desc = TRUE
    return(populationImpact)
  })
  
### Economic impact by state
  output$economicImpact <- renderChart({
    data <- melt(dt.agg.year()[, list(Year=YEAR, Propety=PROPDMG.TOTAL, Crops=CROPDMG.TOTAL)],id='Year')
    economicImpact <- nPlot(
      value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
      type = 'stackedAreaChart', dom = 'economicImpact', width = 650
    )
    economicImpact$chart(margin = list(left = 100))
    economicImpact$yAxis( axisLabel = "Total damage (Million USD)", width = 80)
    economicImpact$xAxis( axisLabel = "Year", width = 70)
    
    return(economicImpact)
  })

## Render data table and create download handler
  output$table <- renderDataTable(
    {dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))
  output$selecteddata <- downloadHandler(filename = 'selecteddata.csv',
                                         content = function(file) 
                                         {
                                           write.csv(dataTable(), file, row.names=FALSE)
                                         }
  )
})