

rm(list = ls())


suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(highcharter)
  library(shinyWidgets)
  library(sf)
  library(leaflet)
  library(raster)
  library(shiny)
  library(leaflet.providers)
})


my_link = 'https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population'
my_df = my_link %>% 
  read_html(x = .) %>% 
  html_node(., ".wikitable") %>% 
  html_table(x = ., fill = TRUE)


my_df = my_df %>% 
  mutate(
    Population = as.numeric(gsub(pattern = ",|(estimated)| |b|[[:punct:]]",
                                 replacement = "",x = Population)),
    `Rural population` = as.numeric(gsub(pattern = ",|(estimated)| |b|[[:punct:]]",
                                 replacement = "",x = `Rural population`)),
    `Urban population` = as.numeric(gsub(pattern = ",|(estimated)| |b|[[:punct:]]",
                                         replacement = "",x = `Urban population`))
  )


india <- st_read("India Shape/india_st.shp")
states_df_v3 = my_df %>% mutate(`State or union territory` = toupper(`State or union territory`)) %>% 
  dplyr::select(STATE = `State or union territory`,
                Population,
                urban_population = `Urban population`,
                rural_population = `Rural population`)

india_df_v3 = inner_join(x = india,y = states_df_v3,"STATE")
india_df_v3 = india_df_v3 %>% 
  mutate(urban_population = as.numeric(urban_population),
         rural_population = as.numeric(rural_population))

my_choices = names(india_df_v3)[2:4]
my_plots = c('Interactive', 'Static')

ui <- fluidPage(
  titlePanel(title = "A choropleth map of provinces in india",windowTitle = "Map of India"),
  sidebarLayout(
    sidebarPanel(
     
      selectInput(inputId = "var_type",
                  label = "Please select variable",
                  choices = my_choices,
                  selected = my_choices[1]
                     
                     ),
      
      
      pickerInput("viz_type","Please select plot type",
                  choices=(c("Interactive", "Static")),
                  selected="Interactive",
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format` = "count > 3"),multiple = F)
      
    ),
    mainPanel(
      
      conditionalPanel(
        condition = "input.viz_type == 'Interactive' ",
        leafletOutput(outputId = "interactive")
      ),
      conditionalPanel(
        condition = "input.viz_type == 'Static'",
        plotOutput(outputId = "static")
      )
    )
  )
)


server <- function(input,output){
  
  compound_data <- reactive({
    india_df_v3
  })
  
  
  myplot_int <- reactive({
    
    df1 <- compound_data()
    
    pal <- colorNumeric(palette = "YlGnBu",domain = india_df_v3$Population)
    
    if(input$var_type == 'Population'){
      df1 %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
       
        addPolygons(fillColor = ~pal(Population),
                    label = ~STATE,
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.3, 
                    smoothFactor = 0.2
        )
    }else if(input$var_type == 'urban_population'){
      df1 %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(fillColor = ~pal(urban_population),
                    label = ~STATE,
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.3, 
                    smoothFactor = 0.2
        )
    }else{
      df1 %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(fillColor = ~pal(rural_population),
                    label = ~STATE,
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.3, 
                    smoothFactor = 0.2
        )
    }
    
  })
  
  output$interactive <- renderLeaflet({
    myplot_int()
    
  })
  
  
  myplot_st <- reactive({
    
    df1 <- compound_data()
    
    if(input$var_type == 'Population'){
      gpl1 <- df1 %>% 
        ggplot()+ 
        geom_sf()+
        geom_sf(data = df1, fill= df1$Population)
      
      gpl1
    }else if(input$var_type == 'urban_population'){
      gpl1 <- df1 %>% 
        ggplot()+ 
        geom_sf()+
        geom_sf(data = df1, fill= df1$urban_population)
      
      gpl1
    }else{
      gpl1 <- df1 %>% 
        ggplot()+ 
        geom_sf()+
        geom_sf(data = df1, fill= df1$rural_population)
      
      gpl1
    }
    
    
  })
  
  output$static <- renderPlot({
    myplot_st()
    
  })
}



shinyApp(ui = ui, server = server)


