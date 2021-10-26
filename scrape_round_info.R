scrape_round_info <- function(tournament_id) {
  options(stringsAsFactors = F)
#tournament_id <- 47867
#test_round <- 1 #for scorecard
#tables <- c('Scores', 'Stat') # for scorecards
#divisions <- c('MPO','FPO') #for scorecards

score_table <- 'pdga-table'

#url <- glue('https://www.pdga.com/apps/tournament/live/event?eventId={tournament_id}&division=MPO&view={tables}&round={test_round}')

url <- glue('https://www.pdga.com/tour/event/{tournament_id}')
#number_of_rounds <- 'class="round-button-group btn-group"'

tournament_html <- read_html(url) 

tournament_results_tables <- tournament_html %>%
  html_nodes('table') %>%
  html_table()

division_scrape <- tournament_html %>%
  html_nodes('h3') %>% html_text() 

divisions_table <- data.frame(
  division = as.character(str_extract(division_scrape, '^.*(?=\\s\\()')),
  n_players = str_extract(division_scrape, '\\(.*\\)')
)

divisions_table <- divisions_table %>%
  mutate(division = str_trim(division, side = "right"))

tournament_results_tables <- purrr::map(tournament_results_tables,~janitor::clean_names(.x),
                                        .id = divisions_table$division)

tournament_status <- tournament_results_tables[[1]] %>% 
  mutate(pro_purse = parse_number(pro_purse))

#first item is the tournament metadata
tournament_results_tables[[1]] <- NULL

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

tournament_results_tables <- 
  purrr::map(tournament_results_tables,
             ~mutate_at(.x,
                     vars(
                       any_of(
                         c('prize', 'total')
                         )
                       ),
                     .funs =  function(x, ...) {
                       if (mode(x) == 'numeric') {
                         x
                       } else {
                         parse_number(x, ...)
                       }
                     }
                     )
             )

tournament_results_tables <- tournament_results_tables %>%
  purrr::map(
    ~mutate(.x, 
            par = parse_par(par))
  )

tournament_results_tables <- set_names(tournament_results_tables, divisions_table$division)

tournament_results_tables <- bind_rows(tournament_results_tables, .id = 'division')

# tournament_html %>%
#   #html_nodes('table') %>%
#   html_text(class = 'division')
# 
# tournament_status <- tournament_html %>%
#   html_node('status') %>% html_text() 


tournament_results_tables

}
