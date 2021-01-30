
library(shiny.semantic)
menu_content <- list(
  list(name = "MAP", link = "#vessel_dropdown", icon="ship",active_location = c("map","table")),
  list(name = "Stats",link = "#port_dropdown",icon="ship"),
  list(name = "Comparison")
)

if (interactive()){
  library(readr)
  library(dplyr)
  library(shiny)
  library(shiny.semantic)
  library(lubridate)
  library(geosphere)
  library(leaflet)
  library(plotly)
  ui <- semanticPage(
    segment(
    horizontal_menu(menu_content),
    class = "placeholder segment"),
    #render_menu_link(location = "#map",title = "MAP",active_location = c("map","table")),
    
    selectInput("vessel_dropdown",label ="Select vessel type:" , choices=ships$ship_type %>% unique(),selected = ships$ship_type %>% unique() %>%  purrr::pluck(1),multiple=F,width = 200),
    #p(),
    selectInput("SHIPNAME_dropdown",label = "Select vessel:", choices=ships$SHIPNAME %>% unique(),selected = ships$SHIPNAME %>% unique() %>%  purrr::pluck(1),multiple=F,width = 200),
    #p("Select year:"),
    selectInput("year_dropdown",label = "Select year:", choices=lubridate::year(ships$DATETIME) %>% unique(),selected = lubridate::year(ships$DATETIME) %>% unique() %>% purrr::pluck(1),multiple=F,width = 200),
    #p("Select month:"),
    selectInput("month_dropdown",label = "Select month:", choices=month.name[lubridate::month(ships$DATETIME) %>% unique()],selected = month.name[lubridate::month(ships$DATETIME) %>% unique()] %>%  purrr::pluck(1),multiple=F,width = 200),
    segment(
    uiOutput("cards"),
    class = "basic",
    a(class="ui green ribbon label", "Observation when vessel sailed the longest distance between two consecutive observations on the map"),
    leafletOutput("map")),
    segment(
    class = "basic",
    a(class="ui green ribbon label", "Observation when vessel sailed the longest distance between two consecutive observations"),
    semantic_DTOutput("table_dist")
    ),
    segment(class = "basic",
            a(class="ui green ribbon label", "More information about vessel"),
    semantic_DTOutput("table"),
    uiOutput("Envbug")),
    segment(
      class = "basic",
      a(class="ui green ribbon label", "Number vessel by type"),
      plotlyOutput("plot")
      
    ),
    ###############port stats
    selectInput("port_dropdown",label ="Select port:" , choices=ships$PORT %>% unique(),selected = ships$PORT %>% unique() %>%  purrr::pluck(1),multiple=F,width = 200),
    selectInput("stat_vessel_dropdown",label ="Select vessel type:" , choices=append(ships$ship_type %>% unique(), "All"),selected = append(ships$ship_type %>% unique(), "All") %>%  purrr::pluck(1),multiple=F,width = 200),
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
  )
  server <- function(input, output, session) {
    observeEvent(input$vessel_dropdown,{
      x <-   ships %>% dplyr::filter(ship_type==input$vessel_dropdown)  %>% dplyr::select(SHIPNAME) %>% unique()
      x <- x$SHIPNAME
      if (is.null(x))
        x <- character(0)
      updateSelectInput(session, "SHIPNAME_dropdown",
                        label = "Select vessel:",
                        choices = x,
                        selected = head(x, 1)
      )
    })
    user_choice <- reactive({
      list(input$vessel_dropdown,
           input$SHIPNAME_dropdown,
           input$year_dropdown,
           input$month_dropdown)
    })
    observeEvent(user_choice(),{
      df <- reactive({
        a =try(ships %>% filter(ship_type==input$vessel_dropdown , SHIPNAME==input$SHIPNAME_dropdown,month.name[lubridate::month(ships$DATETIME)] ==input$month_dropdown,lubridate::year(ships$DATETIME) ==input$year_dropdown) %>% dplyr::select(SHIPNAME,DESTINATION,ship_type,DATETIME,LAT,LON,SPEED,LENGTH,WIDTH,is_parked))
        if(inherits(a, 'try-error')){
          return(NULL)
        }
        else{
          a
        }
      })
      
      distance_df<-reactive({
        df<-df()
        #df <-df %>% dplyr::filter(is_parked==0)
        if(is.data.frame(df)){
        a =try( df %>%
                  dplyr::mutate(lat_prev = lag(LAT,1), lon_prev = lag(LON,1) ) %>%
                  dplyr::mutate(distance = geosphere::distHaversine(matrix(c(lon_prev, lat_prev), ncol = 2),
                                                         matrix(c(LON, LAT),   ncol = 2))) 
                
        )
        if(inherits(a, 'try-error')){
          return(NULL)
        }
        else{
          a
        }
        }
       })
      
      longuest_distance_df<-reactive({
        df<-distance_df()
        if(!is.null(df)){
        a =try(df[c(which.max(df$distance)-1,which.max(df$distance)),])
        if(inherits(a, 'try-error')){
          return(NULL)
        }
        else{
          a
        }
        }
      })
      
      output$cards <- renderUI({
        vessel_info<-distance_df() %>% summarize(distance=sum(distance,na.rm = T))
        vessel_info$SHIPNAME<-input$SHIPNAME_dropdown
        vessel_info$ship_type<-input$vessel_dropdown
        cards(class = "three", vessel_info %>% purrrlyr::by_row(~{
          card(div(class = "content", div(class = "header", 
                                          .$SHIPNAME), div(class = "description", paste("Vessel type:", 
                                                                                        .$ship_type)), div(class = "description", 
                                                                                                           paste("Total distance:", .$distance))))
        }) %>% {
          .$.out
        })
      })
      output$table <- DT::renderDataTable({
        if(is.null(distance_df())){
          output$Envbug <- renderUI(p('Can not display this dataframe! Please verify it and try again.'))
        }
        else{
          output$Envbug <- renderUI(p())
          distance_df()
        }
        
      })
      output$table_dist <- DT::renderDataTable({
        longuest_distance_df()
      })
      map_data<-longuest_distance_df()
      # ### Make ship icon
      # oceanIcons <- iconList(
      #   ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18)
      # )
      output$map <- leaflet::renderLeaflet({ leaflet(data = map_data) %>% addTiles() %>%
          addMarkers(~LON, ~LAT, popup = ~as.character(DATETIME), label = ~as.character(DATETIME))
      })
      
      output$plot <- renderPlotly({
        tp<-as.data.frame(table(ships$ship_type))
        plot_ly(tp, x = ~Var1,y=~Freq, color = ~Var1,type = "bar")%>% layout(
          title = 'Number of vessels by type',
          xaxis = list(
            title = 'Vessels type'
          ),
          yaxis = list(
            title = 'Number of vessels'
            
          )
        )  
      })
  })
    ##############port stats tab
    observeEvent(input$port_dropdown,{
      x <-   ships %>% dplyr::filter(PORT==input$port_dropdown)  %>% dplyr::select(ship_type) %>% unique()
      x <- append(x$ship_type %>% unique(), "All")
      if (is.null(x))
        x <- character(0)
      updateSelectInput(session, "stat_vessel_dropdown",
                        label = "Select vessel:",
                        choices = x,
                        selected = head(x, 1)
      )
    })
    port_stat_choice <- reactive({
      list(input$port_dropdown,
           input$stat_vessel_dropdown)
    })
    observeEvent(port_stat_choice(),{
      port_df<-reactive({
        if(input$stat_vessel_dropdown=="All"){
          df<-ships %>% filter(PORT==input$port_dropdown,is_parked==0) %>% group_by(date,ship_type) %>% 
            summarize(number=n())
        }
        else{
          df<-ships %>% filter(PORT==input$port_dropdown,ship_type==input$stat_vessel_dropdown,is_parked==0) %>% group_by(date) %>% 
            summarize(number=n())
        }
        df
      })
      output$stat_plot_port_all <- renderPlotly({
        df<-port_df()
        if(input$stat_vessel_dropdown=="All"){
        plot_ly(df, x = ~date,y=~number, color = ~ship_type,type = "bar") %>% layout(
          title = 'Total number of vessels (not parked) in the port',
          xaxis = list(
            title = 'Times'
          ),
          yaxis = list(
            title = 'Number of vessels (not parked) in the port'
            
          )
        )
        }
        else{
          plot_ly(df, x = ~date,y=~number,type = "bar")%>% layout(
            title = 'Total number of vessels (not parked) in the port',
            xaxis = list(
              title = 'Times'
            ),
            yaxis = list(
              title = 'Number of vessels (not parked) in the port'
              
            )
          )
        }
      })
      DWT_df<-reactive({
        if(input$stat_vessel_dropdown=="All"){
          df<-ships %>% filter(PORT==input$port_dropdown,is_parked==0) %>% group_by(date,ship_type) %>% 
            summarize(DWT=sum(DWT,na.rm = T))
        }
        else{
          df<-ships %>% filter(PORT==input$port_dropdown,ship_type==input$stat_vessel_dropdown,is_parked==0) %>% group_by(date) %>% 
            summarize(DWT=sum(DWT,na.rm = T))
        }
        df
        
      })
      output$DWT_plot_port_all <- renderPlotly({
        df<-DWT_df()
        if(input$stat_vessel_dropdown=="All"){
          plot_ly(df, x = ~date,y=~DWT, color = ~ship_type,type = "bar")%>% layout(
          title = 'Total capacity of vessels (not parked) in the port',
          xaxis = list(
            title = 'Times'
          ),
          yaxis = list(
            title = 'Sum capacity'
            
          )
        )}
        else{
          plot_ly(df, x = ~date,y=~DWT,type = "bar")%>% layout(
            title = 'Total capacity of vessels (not parked) in the port',
            xaxis = list(
              title = 'Times'
            ),
            yaxis = list(
              title = 'Sum capacity'
              
            )
          )
        }
      })
      })
  }
  shinyApp(ui, server)
}
