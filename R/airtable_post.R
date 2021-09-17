source(here::here("R/social_media.R"))

airtable_base <- "applRcAPoTh7RI9O4"
root_img <- here::here("img")
insta_hashtags <- "
__________________________
📷: www.gospelanalysis.com
📧: gospelanalysisnow@gmail.com - ask us a question!
__________________________
#churchofjesuschristoflatterdaysaints #churchofjesuschrist #hearhim
#generalconference #scripture #scriptures
#lds #ldsquotes #mormon
#jesuschrist #jesus #jesús #christ #savior
#christian #christianity #christianitypost #christianityquote
#discipleship
#data #datascience #dataanalytics #datascientist #dataviz
#religion #faith #faithquotes #faithoverfear
"
twitter_hashtags <- "#jesus #christ #faith #religion"
stopifnot(stringr::str_count(insta_hashtags, "#") <= 30)

get_img_files <- function(slug) {
  path <- file.path(root_img, slug)
  dirs <- list.files(path, full.names = T, recursive = F)
  out <- list()
  for (d in dirs) {
    name <- basename(d)
    p <- file.path("https://www.gospelanalysis.com/img", slug)
    out[[name]]$plot <- file.path(p, name, "plot.png")
    out[[name]]$title <- read_content(slug, name, "title")
    out[[name]]$body <- read_content(slug, name, "body")
  }
  out
}


airtable_post <- function(slug, name = NULL, tab_name = "Scheduled") {
  # specify name if you want to only upload one
  # tab_name = "Testing"
  posts <- get_img_files(slug)

  SocialMediaPosts <-
    airtabler::airtable(
      base = airtable_base,
      tables = c("Scheduled", "Backlog")
    )
  # Read posts
  scheduled <- SocialMediaPosts[[tab_name]]$select()
  backlog <- SocialMediaPosts[['Backlog']]$select()
  already_uploaded_slugs = unique(c(scheduled$slug_name, backlog$slug_name))

  if (!is.null(name)) {
    post_names <- name
  } else {
    post_names <- names(posts)
  }
  for (post in post_names) {
    slug_name <- glue::glue("{slug}-{post}")
    # skip if the slug has already been uploaded
    if (slug_name %in% already_uploaded_slugs) next

    plot_url <- posts[[post]]$plot
    copy_title <- posts[[post]]$title
    copy_body <- posts[[post]]$body

    post_url <- fs::path("https:://www.gospelanalysis.com/posts", slug)

    # Create copy
    insta_fb_copy <- glue::glue("{copy_title}\n{copy_body}\nRead more: {post_url}\n{insta_hashtags}")
    twitter_copy <- glue::glue("{copy_title} {post_url} {twitter_hashtags}")

    new_post <- list(
      created_date = lubridate::now(tz = "US/Pacific"),
      slug_name = slug_name,
      post_url = post_url,
      title = copy_title,
      twitter_copy = twitter_copy,
      insta_fb_copy = insta_fb_copy,
      img_url = plot_url,
      tagged_users = "@gospelanalysis",
      status = "backlog"
      # TODO: add scheduling (requires premium?)
      # scheduled_datetime = ymd_hms("2021-09-06 16:15:00", tz= "US/Pacific")
    )
    resp <- SocialMediaPosts[[tab_name]]$insert(new_post)
    msg = glue::glue("Uploaded to airtable: {slug_name}")
    cli::cli_alert_success(msg)
  }
  if(exists('resp')){
    invisible(resp)
  }
}

airtable_backlog_to_scheduled <- function() {
  SocialMediaPosts <-
    airtabler::airtable(
      base = airtable_base,
      tables = c('Backlog', 'Scheduled')
    )
  backlog <- SocialMediaPosts[['Backlog']]$select() %>%
    arrange(desc(created_date))
  if(nrow(backlog > 0)){
    backlog1 <- backlog %>% head(1)
    backlog1_id <- backlog1$id
    backlog1 <- backlog1 %>% select(-id, -createdTime, -last_modified)


    # Move from backlog to scheduled, delete off backlog
    stopifnot(nrow(backlog1) == 1)
    inserted <- SocialMediaPosts[['Scheduled']]$insert(backlog1)
    SocialMediaPosts[['Backlog']]$delete(backlog1_id)
  }
}

