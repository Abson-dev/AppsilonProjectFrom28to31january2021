# Load packages
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

# Define server function
server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$vessel_dropdown,{
    x <-   ships %>% dplyr::filter(ship_type==isolate(input$vessel_dropdown))  %>% dplyr::select(SHIPNAME) %>% unique()
    x <- x$SHIPNAME
    updateSelectInput(session, inputId="SHIPNAME_dropdown", choices=x)
  })
  observeEvent(input$SHIPNAME_dropdown,{
    df <- reactive({
      a =try(ships %>% filter(ship_type==isolate(input$vessel_dropdown) , SHIPNAME==isolate(input$SHIPNAME_dropdown),month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_dropdown)) %>% dplyr::select(SHIPNAME,DESTINATION,ship_type,DATETIME,LAT,LON,SPEED,LENGTH,WIDTH,is_parked)) #
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
      vessel_info$SHIPNAME<-isolate(input$SHIPNAME_dropdown)
      vessel_info$ship_type<-isolate(input$vessel_dropdown)
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
   })
  ##############port stats tab
  observeEvent(input$port_dropdown,{
    x <-   ships %>% dplyr::filter(port==isolate(input$port_dropdown))  %>% dplyr::select(ship_type) %>% unique()
    x <- c("All",x$ship_type)
    updateSelectInput(session, inputId="stat_vessel_dropdown", choices=x)
  })
  port_stat_choice <- reactive({
    # list(input$port_dropdown,
    #      input$stat_vessel_dropdown,
    #      input$year_stat_c_dropdown,
    #      input$month_stat_c_dropdown)
  })
  observeEvent(input$stat_vessel_dropdown,{
    port_df<-reactive({
      if(input$stat_vessel_dropdown=="All"){
        df<-ships %>% filter(port==isolate(input$port_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_dropdown)) %>% group_by(date,ship_type) %>% 
          summarize(number=n())
      }
      else{
        df<-ships %>% filter(port==isolate(input$port_dropdown),ship_type==isolate(input$stat_vessel_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_dropdown)) %>% group_by(date) %>% 
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
            
          ), barmode = 'stack'
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
        df<-ships %>% filter(port==isolate(input$port_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_dropdown)) %>% group_by(date,ship_type) %>% 
          summarize(DWT=sum(DWT,na.rm = T))
      }
      else{
        df<-ships %>% filter(port==isolate(input$port_dropdown),ship_type==isolate(input$stat_vessel_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_dropdown)) %>% group_by(date) %>% 
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
            
          ), barmode = 'stack'
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
  
  ###########port comparaison
  observeEvent(input$port_c_dropdown,{
    x <- ships$port %>% unique()
    ii <- which(input$port_c_dropdown==x)
    if (length(ii)>0) {
      x <- x[-c(ii)]
      updateSelectInput(session, inputId="port_compare_dropdown", choices=x)
    }
  })
  observeEvent(input$port_c_dropdown,{
    x <-   ships %>% dplyr::filter(port==isolate(input$port_c_dropdown))  %>% dplyr::select(ship_type) %>% unique()
    x <- c("All",x$ship_type)
    updateSelectInput(session, inputId="stat_vessel_c_dropdown", choices=x)
  })
  port_stat_choice_compare <- reactive({
    list(input$stat_vessel_c_dropdown,
         input$port_compare_dropdown)
  })
  observeEvent(port_stat_choice_compare(),{
    port_compare_df<-reactive({
      if(input$stat_vessel_c_dropdown=="All"){
        #First port
        df<-ships %>% filter(port==isolate(input$port_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(number=n())
        df$port<-isolate(input$port_c_dropdown)
        #Second port
        df2<-ships %>% filter(port==isolate(input$port_compare_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(number=n())
        df2$port<-isolate(input$port_compare_dropdown)
        #combine
        df<-rbind(df,df2)
        
      }
      else{
        #First port
        df<-ships %>% filter(port==isolate(input$port_c_dropdown),ship_type==isolate(input$stat_vessel_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(number=n())
        df$port<-isolate(input$port_c_dropdown)
        #second port
        df2<-ships %>% filter(port==isolate(input$port_compare_dropdown),ship_type==isolate(input$stat_vessel_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(number=n())
        df2$port<-isolate(input$port_compare_dropdown)
        #combine
        df<-rbind(df,df2)
      }
      df
    })
    output$compare_port_number <- renderPlotly({
      df<-port_compare_df()
      plot_ly(df, x = ~date,y=~number, color = ~port,type = "bar") %>% layout(
        title = 'Total number of vessels (not parked) in the port',
        xaxis = list(
          title = 'Times'
        ),
        yaxis = list(
          title = 'Number of vessels (not parked) in the port'
          
        )
      )
      
      
    })
    DWT_compare_df<-reactive({
      if(input$stat_vessel_c_dropdown=="All"){
        df<-ships %>% filter(port==isolate(input$port_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date,ship_type) %>% 
          summarize(DWT=sum(DWT,na.rm = T))
        df$port<-isolate(input$port_c_dropdown)
        df2<-ships %>% filter(port==isolate(input$port_compare_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date,ship_type) %>% 
          summarize(DWT=sum(DWT,na.rm = T))
        df2$port<-isolate(input$port_compare_dropdown)
        df<-rbind(df,df2)
      }
      else{
        df<-ships %>% filter(port==isolate(input$port_c_dropdown),ship_type==isolate(input$stat_vessel_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(DWT=sum(DWT,na.rm = T))
        df$port<-isolate(input$port_c_dropdown)
        df2<-ships %>% filter(port==isolate(input$port_compare_dropdown),ship_type==isolate(input$stat_vessel_c_dropdown),is_parked==0,month.name[lubridate::month(ships$DATETIME)] ==isolate(input$month_stat_c_dropdown),lubridate::year(ships$DATETIME) ==isolate(input$year_stat_c_dropdown)) %>% group_by(date) %>% 
          summarize(DWT=sum(DWT,na.rm = T))
        df2$port<-isolate(input$port_compare_dropdown)
        df<-rbind(df,df2)
      }
      df
      
    })
    output$DWT_compare_port <- renderPlotly({
      df<-DWT_compare_df()
      plot_ly(df, x = ~date,y=~DWT, color = ~port,type = "bar")%>% layout(
        title = 'Total capacity of vessels (not parked) in the port',
        xaxis = list(
          title = 'Times'
        ),
        yaxis = list(
          title = 'Sum capacity'
        )
      )
      
    })
  })
})
