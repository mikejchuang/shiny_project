library(maps)
library(tidyr)
library(htmltools)
library(ggplot2)


function(input, output, session){

  ##setting up reactive data to be used in chart and graphs
  
  ChartData <- reactive({
    CD <- raw_slice %>% 
      filter(.,borough %in% input$CDborough) %>%
      filter(., complaint_type %in% input$CDcomplaint_type)
    CD <- CD %>% group_by(., CD[,input$timescale]) %>% arrange(., desc(CD[,input$timescale])) %>% dplyr::summarise(., chartcount=n())
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
  
  HeatDataD <- debounce(HeatData, 2000)

  topHeatData <- reactive({ 
    THD <-  HeatData() %>%
    group_by(., latitude, longitude,complaint_type, incident_address) %>% dplyr::summarize(., top_complaint_count=n()) %>% arrange(.,desc(top_complaint_count))
     THD1 <- THD %>% dplyr::mutate(., total_count=sum(top_complaint_count)) %>% filter(top_complaint_count == max(top_complaint_count)) %>% arrange(.,desc(total_count)) 
   return(head(THD1, 50))
  })
  
  topHeatDataD <- debounce(topHeatData, 2000)

# Output for Interactive Chart
  
  output$charts <- renderPlotly({

   pchart <-  plot_ly(data=ChartData())%>%
      add_trace( x=ChartData()[[1]], y = ~chartcount,
                 type = 'scatter', mode = 'lines', line = list(color = 'rgb(200, 50, 50)', width = 2)) %>% 
    layout(title ="<b>Number of Incidents Over 2015</b>", xaxis = list(title = paste(c('Timescale:'), input$timescale), showticklabels = TRUE),
             yaxis = list(title = "Incident Count"),showlegend = FALSE)
     pchart$elementId <- NULL
      pchart
  })

  #Output for Interactive Map
  
    output$leafheat <- renderLeaflet({

    leaflet(data=HeatDataD())%>%
      addProviderTiles(providers$Stamen.Toner) %>%  
      setView(lat = 40.73, lng = -73.92,zoom = 11)%>% 
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      #removeWebGLHeatmap(layerId = 'h') %>%
     addMarkers( lng=~longitude, lat=~latitude,clusterOptions =markerClusterOptions(), label=~paste( 'Address:', incident_address,'\n',
                                                                                                      'Complaint Type:',complaint_type, '\n'), group='clusters')%>%
      addWebGLHeatmap( lng=~longitude, lat=~latitude, size=75, alphaRange=0.01, group='heatmap') %>%
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
    
    #Info boxes for Interactive Chart
    
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
    
    #Info Boxes and data table for Heat Map
    
    output$heatTotBox <- renderInfoBox({
      heatTot <- dplyr::summarise(HeatDataD(), count=n())
      heat50 <- ungroup(topHeatDataD()) %>% summarise(., tcount=sum(total_count))
      infoBox( h6('Total Number of Incidents Visualized:'), subtitle=HTML(paste('Total Incidents:',tags$b(heatTot), br(),'Top 50 Incident Count:', tags$b(heat50[[1]]))), width=12, color='black')
    })
    output$heatPropBox <- renderInfoBox({
      heatTot <- dplyr::summarize(HeatDataD(), hcount=n())
      heat50 <- ungroup(topHeatDataD()) %>% summarise(., tcount=sum(total_count))
      heatProp= heat50[[1]]/heatTot[1]
      
      infoBox( h5('Proportion of Top_50/Total:'), subtitle=tags$h4(tags$b(heatProp)),icon=icon('balance-scale'), width=12, color='black')
    })
    
    output$topHeatTable <- DT::renderDataTable({
      datatable(topHeatDataD()) %>% 
        formatStyle(input$selected,  
                    background="black", fontWeight='bold')
    })
    
   #Output for entire data set
    
  output$table <- DT::renderDataTable({
    datatable(raw_slice) %>% 
      formatStyle(input$selected,  
                  background="black", fontWeight='bold')
  })
}
