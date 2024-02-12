library(leaflet)
library(ggplot2)
library(dplyr)
library(htmltools)
library(DT)


pipe_names <- pipes$pipeline %>% unique()
pal2 <- colorFactor(viridis_pal(option = "D")(length(pipe_names)), 
                    domain = pipe_names)

function(input, output, session) {
  
  ## Tabla ###########################################
  
  #2 filtro
  observe({
    pipelines <- if (is.null(input$cluster)) character(0) else {
      hyb_dat %>% 
        filter(cluster %in% input$cluster) %>%
        `$`('pipeline') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$pipelines[input$pipelines %in% pipelines])
    updateSelectizeInput(session, "pipelines", choices = pipelines,
                         selected = stillSelected, server = TRUE)
  })
  
  #3 filtro
  observe({
    types <- if (is.null(input$cluster)) character(0) else {
      hyb_dat %>% 
        filter(cluster %in% input$cluster,
               is.null(input$pipelines) | pipeline %in% input$pipelines) %>%
        `$`('type') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$types[input$types %in% types])
    updateSelectizeInput(session, "types", choices = types,
                         selected = stillSelected, server = TRUE)
  })
  
  # 4 filtro
  observe({
    products  <- if (is.null(input$cluster)) character(0) else {
      hyb_dat %>%
        filter(cluster %in% input$cluster,
               is.null(input$pipelines) | pipeline %in% input$pipelines,
               is.null(input$types) | type %in% input$types) %>%
        `$`('producto') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$products[input$products %in% products])
    updateSelectizeInput(session, "products", choices = products,
                         selected = stillSelected, server = TRUE)
  })
  
  df1 <- reactive(
    hyb_dat %>%
      filter(
        is.null(input$cluster)  | cluster %in% input$cluster,
        is.null(input$pipelines)| pipeline %in% input$pipelines,
        is.null(input$types)    | type %in% input$types,
        is.null(input$products) | producto %in% input$products
      )
  )
  
  # tabla final
  output$hyb_table <- renderDataTable({
    
    sketch = withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 6, style = "border-right: solid 2px;", 'Info Genotipo'),
          th(colspan = 4, style = "border-right: solid 2px;", 'Carac. Agronómicos'),
          th(colspan = 3, style = "border-right: solid 2px;", 'Enf. Sistémicas'),
          th(colspan = 8, style = "border-right: solid 2px;", 'Enf. Foliares'),
          th(colspan = 2, style = "border-right: solid 2px;", 'Enf. Tallo'),
          th(colspan = 3, 'Enf. Espigas')
        ),
        tr(
          lapply(colnames(hyb_dat), th)
        )
      )
    ))
    
    df1() %>% 
      datatable(
        # extensions = 'Buttons',
        escape = FALSE, 
        container = sketch,
        rownames = F,
        options = list(
          scrollX = F
          , dom = 'ft'
          , columnDefs = list(list(className = 'dt-center', targets="_all"))
          , lengthChange = F
          , pageLength = nrow(df1())
          # , pagingType = "numbers"  # hides Next and Previous buttons
          , autoWidth = F
          , info = FALSE #  hide the "Showing 1 of 2..." at bottom of table
          , searching = FALSE  # removes search box
          # , dom = 'lfrtipB'
          # , buttons = c("copy", "csv", "pdf")
        )
      )
    
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste(input$cluster, input$pipelines, Sys.Date(), "xlsx", sep=".")
    },
    content = function(file) {
      rio::export(df1(), file)
    }
  )
  
  
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
  
  ## trait Dict ###########################################
  
  #2 organ
  observe({
    organ <- if (is.null(input$group)) character(0) else {
      trait_dat %>% 
        filter(group %in% input$group) %>%
        `$`('organ') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$organ[input$organ %in% organ])
    updateSelectizeInput(session, "organ", choices = organ,
                         selected = stillSelected, server = TRUE)
  })
  
  #3 obs_id
  observe({
    obs_id <- if (is.null(input$group)) character(0) else {
      trait_dat %>%
        filter(group %in% input$group,
               is.null(input$organ) | organ %in% input$organ) %>%
        `$`('obs_id') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$obs_id[input$obs_id %in% obs_id])
    updateSelectizeInput(session, "obs_id", choices = obs_id,
                         selected = stillSelected, server = TRUE)
  })
  
  # tabla final
  output$trait_table <- renderDataTable({
    df2 <- trait_dat %>%
      filter(
        is.null(input$group)  | group %in% input$group,
        is.null(input$organ) | organ %in% input$organ,
        is.null(input$obs_id) | obs_id %in% input$obs_id
      )
    datatable(df2, escape = FALSE)
  })
}

