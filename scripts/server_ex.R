library(leaflet)
library(ggplot2)
library(dplyr)

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    dat %>% 
      filter(FIELD_plantingSeason >= input$range[1] & FIELD_plantingSeason <= input$range[2]) %>% 
      filter(FIELD_pipeline %in% input$pipeline) %>%
      filter(var_ID %in% input$trait)
  })
  
  pal <- colorQuantile(rev(viridis::viridis(10)), dat$scaled_val, n = 5)
  
  output$map <- renderLeaflet({
    
    leaflet(dat) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))%>%
      addLegend("bottomright", pal = pal, 
                values = ~scaled_val,
                title = "Disease press",
                opacity = 1)  %>% 
      addPolygons(data=pipes, weight=1, 
                  color = ~pal2(pipeline),
                  fillOpacity = 0.3, 
                  popup=~pipeline)
  })
  
  observe({
    leafletProxy("map", 
                 data = filteredData(), 
                 session
    ) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = 5,
        fillOpacity = .8,
        color = ~pal(scaled_val), 
        popup = ~paste(FIELD_pipeline, year_loc)
      ) 

  })
  
  observe({ 
    
    filteredData() %>% 
      summarise(
        dis_press = round(median(dis_press, na.rm = T),0), 
        n=n()) -> mu
    
    
    output$histCentile <- renderPlot({
      # If no data are in view, don't plot
      if (nrow(filteredData()) == 0)
        return(NULL)
      
      filteredData() %>% 
        ggplot() +
        aes(x=dis_press) + 
        geom_histogram(fill="steelblue", alpha=.7) + 
        geom_vline(data=mu, aes(xintercept=dis_press), linetype="dashed", col = "red")+
        geom_text(data=mu, aes(x=dis_press, y= Inf,
                               label=paste0(dis_press, " (n=",n, ")")), 
                  vjust=1, hjust=-0.1,
                  col = "red", fontface="bold") + 
        theme_classic() 
    })
  })
  
  ## hybrids ###########################################
  #1
  observe({
    types <- if (is.null(input$pipelines)) character(0) else {
      filter(hyb_dat, pipeline %in% input$pipelines) %>%
        `$`('type') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$types[input$types %in% types])
    updateSelectizeInput(session, "types", choices = types,
                         selected = stillSelected, server = TRUE)
  })
  
  #2
  observe({
    products  <- if (is.null(input$pipelines)) character(0) else {
      hyb_dat %>%
        filter(pipeline %in% input$pipelines,
               is.null(input$types) | type %in% input$types) %>%
        `$`('producto') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$products[input$products %in% products])
    updateSelectizeInput(session, "products", choices = products,
                         selected = stillSelected, server = TRUE)
  })
  
  # #3
  # observe({
  #   products  <- if (is.null(input$pipelines)) character(0) else {
  #     hyb_dat %>%
  #       filter(pipeline %in% input$pipelines,
  #              is.null(input$types) | type %in% input$types) %>%
  #       `$`('producto') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$products[input$products %in% products])
  #   updateSelectizeInput(session, "products", choices = products,
  #                        selected = stillSelected, server = TRUE)
  # })
  
  # tabla final
  output$hyb_table <- DT::renderDataTable({
    df1 <- hyb_dat %>%
      filter(
        is.null(input$pipelines)  | pipeline %in% input$pipelines,
        is.null(input$types) | type %in% input$types,
        is.null(input$products) | producto %in% input$products
      )
    DT::datatable(df1, escape = FALSE)
  })
  
  
  # ## HxF ###########################################
  # #1
  # observe({
  #   types <- if (is.null(input$pipelines)) character(0) else {
  #     filter(hyb_dat, pipeline %in% input$pipelines) %>%
  #       `$`('type') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$types[input$types %in% types])
  #   updateSelectizeInput(session, "types", choices = types,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # #2
  # observe({
  #   products  <- if (is.null(input$pipelines)) character(0) else {
  #     hyb_dat %>%
  #       filter(pipeline %in% input$pipelines,
  #              is.null(input$types) | type %in% input$types) %>%
  #       `$`('producto') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$products[input$products %in% products])
  #   updateSelectizeInput(session, "products", choices = products,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # # tabla final
  # output$hyb_table <- DT::renderDataTable({
  #   df1 <- hyb_dat %>%
  #     filter(
  #       is.null(input$pipelines)  | pipeline %in% input$pipelines,
  #       is.null(input$types) | type %in% input$types,
  #       is.null(input$products) | producto %in% input$products
  #     )
  #   DT::datatable(df1, escape = FALSE)
  # })
  
  ## trait Dict ###########################################
  
  #1
  observe({
    obs_codes <- if (is.null(input$groups)) character(0) else {
      filter(trait_dat, trait_grupo %in% input$groups) %>%
        `$`('obs_code') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$obs_codes[input$obs_codes %in% obs_codes])
    updateSelectizeInput(session, "obs_codes", choices = obs_codes,
                         selected = stillSelected, server = TRUE)
  })
  
  #2
  observe({
    obs_desc <- if (is.null(input$groups)) character(0) else {
      trait_dat %>%
        filter(trait_grupo %in% input$groups,
               is.null(input$obs_codes) | obs_code %in% input$obs_codes) %>%
        `$`('obs_descriptor') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$obs_desc[input$obs_desc %in% obs_desc])
    updateSelectizeInput(session, "obs_desc", choices = obs_desc,
                         selected = stillSelected, server = TRUE)
  })
  
  # tabla final
  output$trait_table <- DT::renderDataTable({
    df2 <- trait_dat %>%
      filter(
        is.null(input$groups)  | trait_grupo %in% input$groups,
        is.null(input$obs_codes) | obs_code %in% input$obs_codes,
        is.null(input$obs_desc) | obs_descriptor %in% input$obs_desc
      )
    DT::datatable(df2,escape = FALSE)
  })
}

