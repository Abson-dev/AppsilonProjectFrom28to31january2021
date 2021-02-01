# Load packages
library(readr)
library(dplyr)
library(shiny)
library(shiny.semantic)
library(lubridate)
library(geosphere)
library(leaflet)
library(plotly)
library(ggplot2)
library(purrr)


ui <- shinyUI(semanticPage(
  tabset(
    tabs = list(
      list(menu = "MAP",
           content = list(
             br(),br(),
             hr(),hr(),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("vessel_dropdown",label ="Select vessel type:" , choices=ships$ship_type %>% unique(),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("SHIPNAME_dropdown",label = "Select vessel:", choices=ships$SHIPNAME %>% unique(),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("year_dropdown",label = "Select year:", choices=lubridate::year(ships$DATETIME) %>% unique(),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("month_dropdown",label = "Select month:", choices=month.name[lubridate::month(ships$DATETIME) %>% unique()],multiple=F,width = 200)),
             br(),br(),
             hr(),hr(),
             segment(
               uiOutput("cards"),
               br(),br(),
               hr(),hr(),
               class = "basic",
               a(class="ui green ribbon label", "Observation when vessel sailed the longest distance between two consecutive observations on the map"),
               leafletOutput("map")),
             br(),br(),
             hr(),hr(),
             segment(
               class = "basic",
               br(),br(),
               hr(),hr(),
               a(class="ui green ribbon label", "Observation when vessel sailed the longest distance between two consecutive observations"),
               semantic_DTOutput("table_dist")
             ),
             segment(class = "basic",
                     br(),br(),
                     hr(),hr(),
                     a(class="ui green ribbon label", "More information about vessel"),
                     semantic_DTOutput("table"),
                     uiOutput("Envbug"))
           ),
           id = "map_distance"),
      list(menu = "Stat",
           content = list(
             br(),br(),
             hr(),hr(),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("port_dropdown",label ="Select port:" , choices=ships$port %>% unique(),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("stat_vessel_dropdown",label ="Select vessel type:" , choices=append(ships$ship_type %>% unique(), "All"),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("year_stat_dropdown",label = "Select year:", choices=lubridate::year(ships$DATETIME) %>% unique(),multiple=F,width = 200)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("month_stat_dropdown",label = "Select month:", choices=month.name[lubridate::month(ships$DATETIME) %>% unique()],multiple=F,width = 200)),
             br(),br(),
             hr(),hr(),
             segment(
               class = "basic",
               a(class="ui green ribbon label", "Total number of vessels (not parked) in the port"),
               plotlyOutput("stat_plot_port_all")
             ),
             segment(
               class = "basic",
               a(class="ui green ribbon label", "Total capacity of vessels (not parked) in the port"),
               plotlyOutput("DWT_plot_port_all")
             )
           ),
           id = "stat_ships"),
      list(menu = "Comparison", content = list(
        br(),br(),
        hr(),hr(),
        div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("port_c_dropdown",label ="Select port:" , choices=ships$port %>% unique(),multiple=F,width = 200)),
        div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("stat_vessel_c_dropdown",label ="Select vessel type:" , choices=append(ships$ship_type %>% unique(), "All"),multiple=F,width = 200)),
        div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("year_stat_c_dropdown",label = "Select year:", choices=lubridate::year(ships$DATETIME) %>% unique(),multiple=F,width = 200)),
        div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("month_stat_c_dropdown",label = "Select month:", choices=month.name[lubridate::month(ships$DATETIME) %>% unique()],multiple=F,width = 200)),
        div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
        div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("port_compare_dropdown",label ="Select port to compare:" , choices=ships$port %>% unique(),multiple=F,width = 200)),
        br(),br(),
        hr(),hr(),
        segment(
          class = "basic",
          a(class="ui green ribbon label", "Total number of vessels (not parked) in the port"),
          plotlyOutput("compare_port_number")
        ),
        br(),br(),
        hr(),hr(),
        segment(
          class = "basic",
          a(class="ui green ribbon label", "Total capacity of vessels (not parked) in the port"),
          plotlyOutput("DWT_compare_port")
        )
      ))
    ),
    active = "map_distance",
    id = "appsilon"
  )
)
)
