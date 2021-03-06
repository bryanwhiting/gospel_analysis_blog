# Functions for formatting paragraphs
here::i_am('R/utils.R')
library(lubridate)
library(glue)
library(magrittr)
library(ggplot2)
library(stringr)

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


my_datatable <- function(df, n=100, dom='t'){
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
      colnames = c("QUOTES (scroll to see more)"),
      rownames=F,
      options = list(
        # dom = "ft",
        dom = dom,
        # deferRender = TRUE,
        pageLength = n,
        scrollY = 250,
        scrollX = FALSE,
        # scroller = TRUE,
        # Sort by invisible column
        # columnDefs = list(
        #   list(orderData = 0, targets = 1),
        #   list(visible = FALSE, targets = 0),
        #   list(className = "dt-head-left", targets = 1)
        # ),
        # font style: https://stackoverflow.com/a/53658138
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#56B4E9', 'color': '#fff'});",
          "}"
        )
      )
    )  %>%
    # TODO: improve style
    # formatStyle(1, fontFamily="IBM Plex Sans")
    # formatStyle(1, fontFamily="Roboto Mono")
    formatStyle(
      columns = colnames(.$x$data),
      fontFamily = "Times New Roman",
      `font-size` = "14px")
}


datatable_quotes <- function(df, n = 20, phrases="xxxx", dom='ft') {

  df <- df %>%
    head(n)

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
    my_datatable(dom=dom)
}

# SEARCHING FUNCTIONS
#'
#' dfg %>%
#'   filter(str_detect(paragraph2, 'read the book of mormon'),
#'          str_detect(paragraph2, 'every day|each day|daily')) %>%
#'   select(pquote) %>%
#'   ask(pquote, terms=c(' da'))
ask <- function(df_text, col, terms, output='dt', n = 200){
  # varname <- deparse(substitute(col))

  x <- df_text
  color = "#56B4E9"
  x$Text <- dplyr::pull(x, {{col}})
  for(t in terms){
    # filter to just search terms
    x <- dplyr::filter(x, stringr::str_detect({{ col }}, t))
    # highlight the terms
    x <- dplyr::mutate(
      x,
      Text = stringr::str_replace_all(
        Text,
        t,
        paste0('<span style="color:', color, '; font-weight:bold">', t, '</span>')
      ))
  }
  x <- dplyr::select(x, -{{col}})
  msg <- paste0('There are ', nrow(x), ' results. Showing ', n, '.')
  message(msg)
  x <- head(x, n)

  if (output == 'view'){
    View(x)
  } else {
    datatable(
      x,
      class = "compact hover",
      extensions = c("Scroller"),
      escape = F,
      caption = tags$caption(
        style = 'caption-side: bottom;',
        withTags(div(HTML('Source: <a href="https://www.churchofjesuschrist.org/study/general-conference">The Church of Jesus Christ of Latter-day Saints</a>')))
      ),
      filter = 'top',
      options = list(
        # dom = "ftp",
        # deferRender = TRUE,
        scrollY = 250,
        # scrollX = F,
        # scroller = TRUE,
        search = list(regex = T),
        pageLength=200,
        # idea copied: https://glin.github.io/reactable/articles/examples.html#language-options
        language = list(paginate = list(
          `first` = "\u276e",
          `previous` = "\u276e",
          `next` = "\u276f"))
      )
    ) %>%
      # formatStyle(1, fontFamily = "IBM Plex Sans") %>%
      # formatStyle(1:ncol(df), fontFamily = "Roboto Mono")
      formatStyle(1:ncol(df), fontFamily = "IBM Plex Sans")
  }
}

gcp <- function(msg){
  gert::git_add(files = ".")
  gert::git_commit_all(message=msg)
  gert::git_push()
}

# Design:
# FB: URL in link + hashtags
# Read more: www.gospelanalysis.com/posts/{slug} - twitter,

