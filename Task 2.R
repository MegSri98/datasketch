# Task 2 
# Loading data
my_link = 'https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population'
my_df = my_link %>% 
  read_html(x = .) %>% 
  html_node(., ".wikitable") %>% 
  html_table(x = ., fill = TRUE)

# Cleaning the data
my_df = my_df %>% 
  mutate(
    Population = as.numeric(gsub(pattern = ",|(estimated)| |b|[[:punct:]]",
                                 replacement = "",x = Population))
  )