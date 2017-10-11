library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(maps)
library(tidyr)
library(htmltools)



function(input, output, session){

  ##setting up Data
  
  ChartData <- reactive({
    CD <- raw_slice %>% 
      filter(.,borough %in% input$CDborough) %>%
      filter(., complaint_type %in% input$CDcomplaint_type)
    CD <- CD %>% group_by(., CD[,input$timescale]) %>% arrange(., desc(CD[,input$timescale])) %>% summarise(., chartcount=n())
    return(CD)
  })
  
  HeatData <- reactive({
    HD <- raw_slice %>%  
      filter(.,borough %in% input$borough) %>%
      filter(., month %in% input$month) %>%
      filter(., weekday %in% input$weekday)%>%
      filter(., hour %in% input$hour) %>%
      filter(., complaint_type %in% input$complaint_type)
    return(HD)
  })
  
  HeatDataD <- debounce(HeatData, 3000)

  topHeatData <- reactive({ 
    THD <-  HeatData() %>%
    group_by(., latitude, longitude,complaint_type, incident_address) %>% summarize(., top_complaint_count=n()) 
   THD1 <- THD %>% mutate(., total_count=sum(top_complaint_count)) %>% filter(top_complaint_count == max(top_complaint_count)) %>% arrange(.,desc(total_count)) #%>% rename(.,complaint_type=top_complaint_type)
    return(head(THD1, 50))
  })
  
  topHeatDataD <- debounce(topHeatData, 3000)


  output$charts <- renderPlotly({

    plot_ly(data=ChartData())%>%
      add_trace( x=ChartData()[[1]], y = ~chartcount,
                 type = 'scatter', mode = 'lines', line = list(color = 'rgb(200, 50, 50)', width = 2)) %>% 
    layout(title ="<b>Number of Incidents Over 2015</b>", xaxis = list(title = paste(c('Timescale:'), input$timescale), showticklabels = TRUE),
             yaxis = list(title = "Incident Count"),showlegend = FALSE)

  })
  # observe({print(input$timescale)})
  #   output$leafmap <- renderLeaflet({
  #     leaflet(raw_slice) %>%
  #       addTiles() %>%
  #       addMarkers(clusterOptions = markerClusterOptions(), popup = ~paste('<b><font color="Black">','Incident Type:', complaint_type,'</font></b><br/>',
  #                                                                          'Date:', created_date,'<br/>',
  #                                                                          'Address:', incident_address,'<br/>',
  #                                                                          'Description', descriptor))
  # })
  
    output$leafheat <- renderLeaflet({

    leaflet(data=HeatDataD())%>%
      addProviderTiles(providers$Stamen.Toner) %>%  
      setView(lat = 40.73, lng = -73.92,zoom = 11)%>% 
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      removeWebGLHeatmap(layerId = 'h') %>%
     addMarkers( lng=~longitude, lat=~latitude,clusterOptions =markerClusterOptions(), label=~paste( 'Address:', incident_address,'\n',
                                                                                                      'Complaint Type:',complaint_type, '\n'), group='clusters')%>%
      addWebGLHeatmap( layerId = 'h',lng=~longitude, lat=~latitude, size=75, alphaRange=0.01, group='heatmap') %>%
       addAwesomeMarkers(data=topHeatDataD(), lng=~longitude, lat=~latitude, icon=icon('map-pin') , group='Top_50', 
                    popup= ~paste('<b><font color="Black">','Address Information:','</font></b><br/>',
                                  'Rank:',dense_rank(desc(total_count)),'<br/>',
                                  'Address:', incident_address,'<br/>',
                                  'Total Number of Complaints:','<b>',total_count,'</b>','<br/>',
                                  'Top Complaint Type:','<b>', complaint_type,'</b>', 'with','<b>', top_complaint_count,'</b>', 'complaints','<br/>')) %>%
      
      addLayersControl(
          baseGroups = c('heatmap','clusters'),
          overlayGroups = c('Top_50'),
          options = layersControlOptions(collapsed = FALSE)) %>%hideGroup('heatmap') %>% hideGroup('Top_50')
                        
    })
    
    output$chartAvgBox <- renderInfoBox({
     
      chartAvg <- round(mean(ChartData()[['chartcount']]), digits = 2)
      chartMed <- round(median(ChartData()[['chartcount']]), digits = 2)
      infoBox( title = tags$b('Measures of the Center:'), subtitle=HTML(paste('Mean Incident Counts:',tags$b(chartAvg), br(), 'Median Incident Counts:', tags$b(chartMed))), icon = icon('hand-o-right'),color='black', width=4)
    })
    output$chartMBox <- renderInfoBox({
      
      chartMax <- ChartData()[which.max(ChartData()$chartcount),]
      chartMin <- ChartData()[which.min(ChartData()$chartcount),]
      chartTot <- round(sum(ChartData()[['chartcount']]), digits = 2)
      infoBox(title= tags$b('Minimums and Maximums'),subtitle=HTML(paste('Total Incidents:',tags$b(chartTot), br(), 'Max Incidents:',tags$b(chartMax[,2]),'occuring on:', tags$b(as.character(chartMax[[1]])), br(),'Min Incidents:',tags$b(chartMin[,2]),'occuring on:', tags$b(as.character(chartMin[[1]])))), icon = icon("hand-o-up"),color='black', width=12)
    })
    
    output$heatTotBox <- renderInfoBox({
      heatTot <- summarise(HeatDataD(), count=n())
      heat50 <- ungroup(topHeatDataD()) %>% summarise(., tcount=sum(total_count))
      infoBox( h6('Total Number of Incidents Visualized:'), subtitle=HTML(paste('Total Incidents:',tags$b(heatTot), br(),'Top 50 Incident Count:', tags$b(heat50[[1]]))), width=12, color='black')
    })
    output$heatPropBox <- renderInfoBox({
      heatTot <- summarise(HeatDataD(), hcount=n())
      heat50 <- ungroup(topHeatDataD()) %>% summarise(., tcount=sum(total_count))
      heatProp= heat50[[1]]/heatTot[1]
      
      infoBox( h5('Proportion of Top_50/Total:'), subtitle=tags$h4(tags$b(heatProp)),icon=icon('balance-scale'), width=12, color='black')
    })
    output$topHeatTable <- DT::renderDataTable({
      datatable(topHeatDataD()) %>% 
        formatStyle(input$selected,  
                    background="black", fontWeight='bold')
    })
    
    # group_selects <- reactiveValues(value = NULL)
    # observe({
    #   filters <- c('borough', 'month', 'weekday', 'hour','complaint_type')
    #   for (i in filters)
    #     if(!is.null(input$filters[i]) || input$filters[i] != '')
    #       group_selects <- c(group_selects, filters[i])
    #   }
    # })
    # 
    # 
    #  observe({
    #   p
    #   
    #   leafletProxy("leafheat", data = data=raw_slice %>% 
    #                  filter(.,borough %in% input$borough) %>%
    #                  #filter(.,year== input$year) %>%
    #                  filter(., month %in% input$month) %>%
    #                  filter(., weekday %in% input$weekday)%>%
    #                  filter(., hour %in% input$hour) %>%
    #                  filter(., complaint_type %in% input$complaint_type)) %>%
    #                 group_by(., input$borough, input$month, input$weekday, input$hour)
    #     
    #     clearShapes() %>%
    #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
    #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
    #     )
    # })
    # 

  output$table <- DT::renderDataTable({
    datatable(raw_slice) %>% 
      formatStyle(input$selected,  
                  background="black", fontWeight='bold')
  })
}
    # Highlight selected column using formatStyle
    # show statistics using infoBox
  #   
  #   output$maxBox <- renderInfoBox({
  #     max_value <- max(state_stat[,input$selected])
  #     max_state <- 
  #       state_stat$state.name[state_stat[,input$selected]==max_value]
  #     infoBox(max_state, max_value, icon = icon("hand-o-up"))
  #   })
  #   output$minBox <- renderInfoBox({
  #     min_value <- min(state_stat[,input$selected])
  #     min_state <- 
  #       state_stat$state.name[state_stat[,input$selected]==min_value]
  #     infoBox(min_state, min_value, icon = icon("hand-o-down"))
  #   })
  #   output$avgBox <- renderInfoBox(
  #     infoBox(paste("AVG.", input$selected),
  #             mean(state_stat[,input$selected]), 
  #             icon = icon("calculator"), fill = TRUE))
  # }
  # 
