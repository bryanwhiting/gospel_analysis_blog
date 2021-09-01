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
quote_paragraph <- function(df, phrase = "xxxx"){
  # assert n rows is 1
  date <- df$date %>% format("%b %Y")
  text <- str_replace(df$paragraph, phrase, glue("**{phrase}**"))
  title <- glue("[{df$title1}, {date}]({df$deep_link})")
  msg <- glue("\n\n>{text}\n\n\n>{df$author1} _{title}_\n\n")
  cat(msg)
}
