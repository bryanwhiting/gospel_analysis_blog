# Workflow:
# I want to save out more plots than I'll have in my rmarkdown
# e.g., I'll create 5 figures for social media, but only
# use 3 in the doc (to prevent overwhelming).
#
# Chunk:
# slug <- basename(getwd())
# p <- ggplot2()
# copy() function adds hashtags
# save_plot(p, copy("hello world"), slug=slug, name='test')
#
# Output:
# img/{slug}/{name1}/plot.png
# img/{slug}/{name1}/copy.txt
# img/{slug}/{name2}/plot.png
# img/{slug}/{name2}/copy.txt
#
# Looper:
# get_files <- function(slug){}
# ${name1}$plot_path.png
# ${name1}$copy
# ${name2}$...
#
# Airtable:
# for each name in get_files()$list, upload image + copy

# TODO: hide this in a yaml file
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
stopifnot(str_count(insta_hashtags, "#") <= 30)


save_img <- function(img, slug, name, is_quote = F) {
  path <- fs::path(root_img, slug, name, "plot.png")
  # TODO: add option for "SQUARE" vs. "NOT SQUARE" (preserve original dimensions)
  if (class(img)[2] == "ggplot") {
    if (is_quote) {
      ggplot2::ggsave(filename = path, plot = img, width = 3, height = 3)
    } else {
      ggplot2::ggsave(filename = path, plot = img, width = 6, height = 6)
    }
  } else if (class(img)[1] == "gt_tbl") {
    gt::gtsave(img, path)
  }
}

write_content <- function(content, slug, name, filename) {
  dir <- fs::path(root_img, slug, name)
  path <- fs::path(dir, filename, ext = "txt")
  dir.create(dir, recursive = T, showWarnings = F)
  write_lines(x = content, file = path)
}

# airtable_write_copy('the-names-of-christ', 'quote', copy=glue('hello world\n{hashtags}'))
read_content <- function(slug, name, filename) {
  path <- fs::path(root_img, slug, name, filename, ext = "txt")
  readLines(path) %>%
    paste0(collapse = "\n")
}
# airtable_read_copy('the-names-of-christ', 'quote')

make_post <- function(img, title, body, slug, name, is_quote = F) {
  save_img(img = img, slug = slug, name = name, is_quote = is_quote)
  stopifnot(str_length(title) <= 200)
  write_content(content = title, slug = slug, name = name, filename = "title")
  write_content(content = body, slug = slug, name = name, filename = "body")
}

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
      tables = c(tab_name)
    )
  # Read posts
  # scheduled <- SocialMediaPosts[[tab_name]]$select()

  if (!is.null(name)) {
    post_names <- name
  } else {
    post_names <- names(posts)
  }
  for (post in post_names) {
    slug_name <- glue("{slug}-{post}")
    plot_url <- posts[[post]]$plot
    copy_title <- posts[[post]]$title
    copy_body <- posts[[post]]$body

    post_url <- fs::path("https:://www.gospelanalysis.com/posts", slug)

    # Create copy
    insta_fb_copy <- glue("{copy_title}\n{post_url}\n{copy_body}\n{insta_hashtags}")
    twitter_copy <- glue("{copy_title} {post_url} {twitter_hashtags}")

    new_post <- list(
      created_date = now(tz = "US/Pacific"),
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
    msg = glue("Uploaded to airtable: {slug_name}")
    cli::cli_alert_success(msg)
  }
  invisible(resp)
}


# TODO: update zapier so it pulls from png directly
deploy <- function(msg){
  rmarkdown::render_site()
  gert::git_add(".")
  gert::git_commit_all(msg)
  gert::git_push()
  # gert::git_push()
}

post <- function(slug, name = NULL, msg = NULL) {
  if(is.null(msg)){
    msg <- glue("deploy and post {slug} {name}") %>% as.character()
  }
  deploy(msg)
  # TODO:
  # cli::cli_progress_bar()
  print('sleeping 10 seconds')
  Sys.sleep(10)
  airtable_post(slug, name)
}

