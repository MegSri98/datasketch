
rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(highcharter)
  library(sf)
  library(leaflet)
  library(raster)
})


my_link = 'https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population'
my_df = my_link %>% 
  read_html(x = .) %>% 
  html_node(., ".wikitable") %>% 
  html_table(x = ., fill = TRUE)


my_df = my_df %>% 
  mutate(
    Population = as.numeric(gsub(pattern = ",|(estimated)| |b|[[:punct:]]",
                                 replacement = "",x = Population))
  )

#  static with ggplot 
my_df %>% 
  ggplot(aes(x = `State or union territory`,y = Population))+
  theme_bw()+
  geom_col(fill = 'blue')+
  coord_flip()+
  labs(x = '',
       title = 'The density of provinces in india')+
  theme(plot.title = element_text(hjust = 0.5))


# interactive using highcharter
my_df %>%
  hchart('bar', hcaes(x = `State or union territory`,y = Population)) %>%
  hc_title(text = "The density of provinces in india") %>% 
  hc_xAxis(title = list(text = ""))

# Indian Map --------------------------------------------------------------
india <- st_read("India Shape/india_st.shp")
states_df_v2 = my_df %>% mutate(`State or union territory` = toupper(`State or union territory`)) %>% 
  dplyr::select(STATE = `State or union territory`, Population)

india_df = inner_join(x = india,y = states_df_v2,"STATE")

# Using leaflet
leaflet(india_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(#fillColor = ~pal(Population),
              label = ~STATE,
              color = "#b2aeae",
              fillOpacity = 0.7, 
              weight = 0.3, 
              smoothFactor = 0.2
  )
# Using ggplot
ggplot(india_df)+ geom_sf()



interactive_static = function(data, provinces = c('ASSAM','BIHAR'),interactive = FALSE){
  pl_df = data %>% filter(STATE %in% provinces)
  
  if(interactive == TRUE){
    leaflet(pl_df) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  #fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  
  }else{
    ggplot(pl_df)+ geom_sf()
  }
}


# interactive
interactive_static(data = india_df,interactive = TRUE,provinces = india_df$STATE[1:10])

# interactive
interactive_static(data = india_df,interactive = FALSE,provinces = india_df$STATE[1:10])


