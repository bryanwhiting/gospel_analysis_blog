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
  readr::write_lines(x = content, file = path)
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

