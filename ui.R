library(shiny)
library(shinythemes)
library(plotly)

naics_list <- c("All","Veterinary Services",
                "Offices of Physicians",
                "Offices of Dentists",
                "Offices of Other Health Practitioners",
                "Outpatient Care Centers",
                "Medical and Diagnostic Laboratories",
                "Home Health Care Services",
                "General Medical and Surgical Hospitals",
                "Skilled Nursing Facilities",
                "Mental Health Facilities",
                "Assisted Living Facilities")

shinyUI(navbarPage("US Medical Institutions - 2014 Census",theme = shinytheme("sandstone"),
                   id = "inTabset",
                   
                   tabPanel("Home", value = "homePanel",
        
                   tags$style("
                              body {
                              -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
                              zoom: 0.9; /* Other non-webkit browsers */
                              zoom: 85%; /* Webkit browsers */
                              }
                              "),
                   
                   mainPanel(
                     column(8,
                            plotOutput("bubPlot", width="100%", height="670px")),
                     column(2,
                            fluidRow(selectInput('naics_filter','Select Category',naics_list)),
                            fluidRow(tableOutput('dtlTable'),
                                     tags$style(type="text/css", "#dtlTable tr:last-child {font-weight:bold;}")),
                            fluidRow(textOutput('expln', inline = T))
                     ), width='500px')
                   
                   ),
                   
                   tabPanel(title = "Pies", value = "piePanel", 
                            column(6,plotlyOutput('estbPie', width="100%", height="500px")),
                            column(6,plotlyOutput('emplPie', width="100%", height="500px")))
                   
                   ))
