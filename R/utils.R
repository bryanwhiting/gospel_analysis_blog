# Functions for formatting paragraphs
library(airtabler)
library(lubridate)
library(glue)
library(magrittr)
library(ggplot2)
root_posts = "/home/rstudio/gospel_analysis_blog/_posts"
root_img = "/home/rstudio/gospel_analysis_blog/img"
hashtags = "
__________________________
📷: www.gospelanalysis.com
📧: gospelanalysisnow@gmail.com - ask us a question!
__________________________
#churchofjesuschrist #churchofjesuschristoflatterdaysaints #genconf #generalconference #scripture #scriptures #scriptureoftheday
#lds #ldsmemes #ldsquotes #ldsart #mormon #mormonmemes #mormons

#jesuschrist #jesus #christ #christian #christianity #jesús #jesuslovesyou
#disciple #discipleship #discipleofchrist #christianitypost #christianityquote #followingjesus #followingjesustogether #hearhim

#data #datascience #dataanalytics #datascientist #datavisualization

#religion #faith #faithquotes #faithblogger #faithwriter
"

#' Quote Paragraph
#'
#' Requires `{r, results="asis"}`
#'
#' @param df
#' @param phrase
#'
#' @return
#' @export
#'
#' @examples
quote_paragraph <- function(df, phrase = "xxxx") {
  # assert n rows is 1
  date <- df$date %>% format("%b %Y")
  text <- str_replace(
    df$paragraph, phrase,
    glue('**{phrase}**')
  )
  title <- glue("[{df$title1}, {date}]({df$deep_link})")
  msg <- glue("\n\n>{text}\n\n\n>{df$author1} _{title}_\n\n")
  cat(msg)
}

quote_verse <- function(df, phrase = "xxxx") {

  # TODO: add deep link
  # assert n rows is 1
  text <- str_replace(
    df$text[1],
    phrase,
    # glue('<strong style="color:{colors[1]}">{phrase}</strong>')
    glue('**{phrase}**')
  )
  source <- glue("{df$book_short_title} {df$chapter_number}:{df$verse_number}")
  msg <- glue("\n\n> {text} ({source}) \n\n")
  cat(msg)
}


datatable_quotes <- function(df, n = 20, phrases="xxxx") {

  df <- df %>%
    head(n) %>%
    select(pquote)

  # could have multiple phrases: hear Him, Hear Him, hear him
  for (p in phrases){
    df <- df %>%
    mutate(
      pquote = str_replace(
        pquote,
        p,
        # glue('<strong style="color:{colors[1]}">{p}</strong>')
        glue('<strong>{p}</strong>')
      )
    )
  }
  df %>%
    datatable(
      class = "compact hover",
      # extensions = c("Scroller", "Responsive"),
      extensions = c("Scroller"),
      escape = F,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: center;",
        withTags(div(HTML('Source: <a href="https://www.churchofjesuschrist.org">ChurchOfJesusChrist.org</a>')))
      ),
      colnames = c("QUOTES (click + scroll to see more)"),
      options = list(
        # dom = "ft",
        dom = "t",
        # deferRender = TRUE,
        pageLength = n,
        scrollY = 250,
        scrollX = FALSE,
        # scroller = TRUE,
        columnDefts = list(
          list(orderDAta = 0, targets = 1),
          list(visible = FALSE, targets = 0),
          list(className = "dt-head-left", targets = 1)
        ),
        # font style: https://stackoverflow.com/a/53658138
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#56B4E9', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
    # TODO: improve style
    # formatStyle(1, fontFamily="IBM Plex Sans")
    # formatStyle(1, fontFamily="Roboto Mono")
    formatStyle(
      columns = colnames(.$x$data),
      fontFamily = "Times New Roman",
      `font-size` = "14px")
}


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
save_img <- function(img, slug, name){
  if(class(img)[2] == 'ggplot'){
    path = glue("{root_img}/{slug}/{name}/plot.png")
    ggsave(filename=path, plot=img, width=3, height=3)
  }
}

save_copy <- function(slug, name, copy){
  dir = glue("{root_img}/{slug}/{name}")
  dir.create(dir, recursive=T, showWarnings=F)
  copy_path = glue("{dir}/copy.txt")
  write(x = copy, file = copy_path)
}

copy <- function(copy, hashtags){
  glue("{copy}\n\n{hashtags}")
}


# airtable_write_copy('the-names-of-christ', 'quote', copy=glue('hello world\n{hashtags}'))

read_copy <- function(slug, name){
  path = glue("{root_img}/{slug}/{name}/copy.txt")
  readLines(path) %>%
    paste0(collapse = "\n")
}
# airtable_read_copy('the-names-of-christ', 'quote')

get_img_files <- function(slug){
  path = file.path(root_img, slug)
  dirs <- list.files(path, full.names = T, recursive=F)
  out <- list()
  for(d in dirs){
    name = basename(d)
    p = file.path("https://www.gospelanalysis.com/img", slug)
    out[[name]]$plot = file.path(p, name, 'plot.png')
    out[[basename(d)]]$copy = read_copy(slug, name)
  }
  out
}

upload_to_airtable <- function(slug, chunk){
  plot_path = glue("https://www.gospelanalysis.com/posts/{slug}/{slug}_files/figure-html5/{chunk}-1.png")

  SocialMediaPosts <-
    airtable(
      base = "applRcAPoTh7RI9O4",
      tables = c("Scheduled")
    )
  scheduled <- SocialMediaPosts$Scheduled$select()

  new_post <- list(
    created_date = now(tz = "US/Pacific"),
    attachment_url = ,
    copy = airtable_read_copy(slug, chunk),
    tagged_users = "@gospelanalysis",
    status = "backlog",
    scheduled_datetime = ymd_hms("2021-09-06 16:15:00", tz= "US/Pacific")
  )
  resp <- SocialMediaPosts$Scheduled$insert(new_post)

}
