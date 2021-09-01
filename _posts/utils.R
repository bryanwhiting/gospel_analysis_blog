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
    glue('<strong style="color:{colors[1]}">{phrase}</strong>')
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
    glue('<strong style="color:{colors[1]}">{phrase}</strong>')
  )
  source <- glue("{df$volume_short_title} {df$chapter_number}:{df$verse_number}")
  msg <- glue("\n\n> {text}({source}) \n\n")
  cat(msg)
}
