library(leaflet)
library(DT)

navbarPage(
  "Perfil defensivo", id="nav",
  
  
  tabPanel(
    "Scores",
    fluidRow(
      column(2, 
             selectInput(
               inputId="cluster",
               label="Cluster",
               c(unique(hyb_dat$cluster)),
               multiple=TRUE)
      ),
      column(2,
             conditionalPanel(
               "input.cluster",
               selectInput("pipelines",
                           "Pipeline",
                           c(unique(hyb_dat$pipeline)),
                           multiple=TRUE)
             )),             
      column(2,
             conditionalPanel(
               "input.cluster",
               selectInput("types",
                           "Crop Type",
                           c(unique(hyb_dat$type)),
                           multiple=TRUE)
             )),
      column(2,
             conditionalPanel(
               "input.cluster",
               selectInput("products",
                           "Product name",
                           c(unique(hyb_dat$producto)),
                           multiple=TRUE)
             )) 
      
    ),
    
    downloadButton("download1","Download xlsx"),
    
    hr(),
    
    dataTableOutput("hyb_table")
    
  ), 
  
  
  tabPanel(
    "Raw data",
    div(class="outer",
        
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(
          top = 10, right = 10,
          sliderInput("range", "Season", 
                      min(dat$FIELD_plantingSeason), 
                      max(dat$FIELD_plantingSeason),
                      value = range(dat$FIELD_plantingSeason), 
                      step = 1, sep=""
          ),
          selectInput("trait", "Trait", unique(dat$var_ID)),
          selectInput("pipeline", "Pipeline", unique(dat$FIELD_pipeline)),
          plotOutput("histCentile", height = 200)
        )
        
        # tags$div(id="cite",
        #   'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
        # )
    )
  ),
  
  tabPanel(
    "Traits",
    fluidRow(
      column(2,
             selectInput(inputId="group",
                         label="Group",              
                         c(unique(trait_dat$group)),  
                         multiple=TRUE)
      ),
      
      column(2,
             conditionalPanel("input.group",
                              selectInput("organ",
                                          "organ",
                                          c(unique(trait_dat$organ)), 
                                          multiple=TRUE)
             )),
      column(2,
             conditionalPanel("input.group",
                              selectInput("obs_id",
                                          "obs_id",
                                          c(unique(trait_dat$obs_id)),  
                                          multiple=TRUE)
             ))
    ),
    hr(),
    DT::dataTableOutput("trait_table")
  )
)



