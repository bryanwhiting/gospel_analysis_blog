# Functions for formatting paragraphs

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
