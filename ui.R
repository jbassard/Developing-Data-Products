# User-Interface coding for a Shiny web application.

if (!require("shiny")) {
  install.packages("shiny")}
if (!require("data.table")) {
  install.packages("data.table")}
if (!require("devtools")) {
  install.packages("devtools")}
if (!require("Rcpp")) {
  install.packages("Rcpp")}
if (!require("rCharts")) { 
  install_github('ramnathv/rCharts')}
library(shiny)
library(data.table)
library(devtools)
library(Rcpp)
library(rCharts)

shinyUI(
  navbarPage("NOAA's Database Explorer",tabPanel("Chart",
                                                
                                                #Control panel
                                                sidebarPanel(sliderInput("range", "Range:", min = 1950, max = 2011, value = c(1950, 2011),format="####"),uiOutput("evtypeControls"),
                                                actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("check-square")),
                                                actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square-o"))),
                                                
                                                #Plot data by US State
                                                mainPanel(tabsetPanel(tabPanel('Plot Data By US state',
                                                                               column(3,wellPanel(radioButtons("populationCategory", "Population impacted category:",c("Both" = "both", "Injuries" = "injuries", "Fatalities" = "fatalities")))),
                                                                               column(3,wellPanel(radioButtons("economicCategory","Economic impact category:", c("Both" = "both", "Property damage" = "property", "Crops damage" = "crops")))),
                                                                               column(7,plotOutput("populationImpactByState"),plotOutput("economicImpactByState"))),
                                                                      
                                                                      
                                                #Plot data by year
                                                                      tabPanel('Plot Data By year',
                                                                               h4('Number of Events by Year', align = "center"),showOutput("eventsByYear", "nvd3"),
                                                                               h4('Population Impacted by Year', align = "center"),showOutput("populationImpact", "nvd3"),
                                                                               h4('Economical Impact by Year', align = "center"),showOutput("economicImpact", "nvd3")),
                                                                      
                                                #Download Tab                                                                      
                                                                      tabPanel('Download Selected Data',dataTableOutput(outputId="table"),downloadButton('downloadData', 'Download Selected Data'))))),
             #Details about the App 
                                        tabPanel("About This Shiny App",mainPanel(includeMarkdown("README.md"))))
)