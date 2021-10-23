library(tidyverse)
library(httr)
library(glue)
library(rvest)

tournament_id <- 47867
test_round <- 1
tables <- c('Scores', 'Stat')
divisions <- c('MPO','FPO')

score_table <- 'pdga-table'

url <- glue('https://www.pdga.com/apps/tournament/live/event?eventId={tournament_id}&division=MPO&view={tables}&round={test_round}')

url <- glue('https://www.pdga.com/tour/event/{tournament_id}')
#number_of_rounds <- 'class="round-button-group btn-group"'

tournament_html <- read_html(url) 

tournament_results_tables <- tournament_html %>%
  html_nodes('table') %>%
  html_table()

tournament_results_tables <- purrr::map(tournament_results_tables,~janitor::clean_names(.x))

tournament_results_tables <- 
  purrr::map(tournament_results_tables,  
           ~rename_at(.x, vars(starts_with('x')),
                      function(x) {
                        if_else(grepl('_', x) == F,
                                'rd1_rating',
                                paste0('rd', str_extract(x, '[:digit:]+'), '_rating'))
                        }
                      )
           )

# tournament_html %>%
#   #html_nodes('table') %>%
#   html_text(class = 'division')
# 
# tournament_status <- tournament_html %>%
#   html_node('status') %>% html_text() 

tournament_status <- tournament_results_tables[[1]] %>% 
  mutate(pro_purse = parse_number(pro_purse))

tournament_results_tables[[1]] <- NULL


division_scrape <- tournament_html %>%
  html_nodes('h3') %>% html_text() 

divisions_table <- data.frame(
  division = str_extract(division_scrape, '^.*(?=\\s\\()'),
  n_players = str_extract(division_scrape, '\\(.*\\)')
)