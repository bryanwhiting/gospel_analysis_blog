# Test airtabler
library(airtabler)
library(lubridate)
library(glue)

SocialMediaPosts <-
  airtable(
    base = "applRcAPoTh7RI9O4",
    tables = c("Scheduled")
  )
scheduled <- SocialMediaPosts$Scheduled$select()

new_post <- list(
  created_date = now(tz = "US/Pacific"),
  attachment_url = "https://www.gospelanalysis.com/posts/second-coming/second-coming_files/figure-html5/unnamed-chunk-3-1.png",
  copy = glue("test at 3:36pm"),
  tagged_users = "@gospelanalysis",
  status = "backlog",
  scheduled_datetime = ymd_hms("2021-09-06 16:15:00", tz= "US/Pacific")
)
resp <- SocialMediaPosts$Scheduled$insert(new_post)


