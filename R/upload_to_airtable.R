library(dplyr)
source(here::here("R/airtable_post.R"))
# upload all slugs
# TODO: upload only most recent?
# fs::dir_ls(here::here('img'))
all_slugs <- list.dirs(here::here('img'), recursive = F) %>%
  basename()
for(s in slug){
  airtable_post(s)
}

