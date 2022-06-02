# Task 3
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