scrape_events <- function(event_year, event_month){
# events ------------------------------------------------------------------

#event_year <- 2021
#event_month <- 09

event_url <- glue('https://www.pdga.com/tour/events/{event_year}/{event_month}')

event_list_html <- read_html(paste0(event_url))

event_tables <- event_list_html %>%
  html_nodes('details') %>%
  html_table() %>%
  bind_rows() %>% 
  janitor::clean_names()

# event_ids <- event_list_html %>% 
#   html_element('tbody')

event_ids <- event_list_html %>% 
  html_nodes('.name') %>% 
  html_nodes('a') %>% 
  html_attr('href') %>%
  str_extract(pattern = '[:digit:]+')

event_tables <- event_tables %>%
  mutate(event_id = event_ids,
         event_url = paste0('https://www.pdga.com/tour/event/', event_id),
         event_year,
         event_month
         )

event_tables
}
