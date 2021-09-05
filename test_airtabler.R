# Test airtabler
library(airtabler)

SocialMediaPosts <-
  airtable(
    base = "app4FiCy5NRkbihg8",
    tables = c("Scheduled")
  )
scheduled <- SocialMediaPosts$Scheduled$select()

record_data <- list(
  Date = "New hotel",
  `Price/night` = 200,
  Stars = "****",
  Amenities = c("Hiking", "Gym"),
  Notes = "Just a sample record.\nWith extra line in notes."
)
new_post <- list(
  date = '2021-09-04',
  # attachments = qplot('hi'),
  attachment_url = "https://www.gospelanalysis.com/posts/second-coming/second-coming_files/figure-html5/unnamed-chunk-3-1.png",
  copy = "testing out some automation",
  tagged_users = ""
)
resp <- SocialMediaPosts$Scheduled$insert(new_post)


