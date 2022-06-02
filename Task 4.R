# Task 4
india <- st_read("India Shape/india_st.shp")
states_df_v2 = my_df %>% mutate(`State or union territory` = toupper(`State or union territory`)) %>% 
  dplyr::select(STATE = `State or union territory`, Population)

india_df = inner_join(x = india,y = states_df_v2,"STATE")

# Using leaflet
leaflet(india_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(#fillColor = ~pal(Population),
    label = ~STATE,
    color = "#b2aeae",
    fillOpacity = 0.7, 
    weight = 0.3, 
    smoothFactor = 0.2
  )
# Using ggplot
ggplot(india_df)+ geom_sf()