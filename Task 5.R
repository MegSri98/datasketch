# Task 5
# Creating function

interactive_static = function(data, provinces = c('ASSAM','BIHAR'),interactive = FALSE){
  pl_df = data %>% filter(STATE %in% provinces)
  # Intercative part
  if(interactive == TRUE){
    leaflet(pl_df) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  #fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
    # Static
  }else{
    ggplot(pl_df)+ geom_sf()
  }
}


# interactive
interactive_static(data = india_df,interactive = TRUE,provinces = india_df$STATE[1:10])

# interactive
interactive_static(data = india_df,interactive = FALSE,provinces = india_df$STATE[1:10])

