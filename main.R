library(tidyverse)
library(httr)
library(glue)
library(rvest)
library(lubridate)


start_date <- '2018-01-01'
current_date <- as.Date(lubridate::today())

seq_dates <- seq.Date(as.Date(start_date), current_date, by = "month")
#seq_dates <- format(seq_dates, "%Y-%m")

tournaments <- purrr::map2(lubridate::year(seq_dates),
                           lubridate::month(seq_dates),
                           scrape_events) %>%
  bind_rows() 

# tournament pages are sorted by weeks in a month.
# if a tournament is in, for example week 4 of january, 
# but starts on feb 1, it'll be on two different event lists
tournaments <- tournaments %>% 
  group_by_at(vars(-event_year, -event_month)) %>% 
  filter(event_month == max(event_month))


#tournaments without par column (C tiers?) causing error
tournaments_round_info <- purrr::map(tournaments$event_id,
                                     scrape_round_info)











# tournements over time ---------------------------------------------------


tournaments %>% 
  group_by(event_year, event_month) %>% 
  summarize(n = n()) %>% 
  mutate(
    event_date = as.Date(paste0(
      event_year, '-', str_pad(event_month, 2, 'left', 0), '-01' )
    )) %>% 
  ggplot() + geom_line(aes(x = event_date, y = n, group = 1)) + scale_x_date()
