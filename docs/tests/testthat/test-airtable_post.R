# test_file('tests/testthat/test-airtable_post.R')
source(here::here("R/airtable_post.R"))
root_img <- here::here("img")
dir_t <- here::here("img/test")
dir_tt <- here::here("img/test/test")
slug <- "test"
name <- "test"

teardown <- function() {
  unlink(dir_t, recursive = T)
}


test_that("get_img_files() works", {
  p <- ggplot2::ggplot()
  save_img(p, slug, "dir1")
  write_content("hello1", slug, "dir1", "title")
  write_content("hello2", slug, "dir1", "body")
  save_img(p, slug, "dir2")
  write_content("hello3", slug, "dir2", "title")
  write_content("hello4", slug, "dir2", "body")
  ans <- get_img_files(slug)
  expect_equal(length(ans), 2)
  expect_equal(ans$dir1$plot, "https://www.gospelanalysis.com/img/test/dir1/plot.png")
  expect_equal(ans$dir2$title, "hello3")
  expect_equal(ans$dir2$body, "hello4")
  teardown()
})


test_that("airtable_post() works", {
  p <- ggplot2::ggplot()
  make_post(img = p, title = "title", body = "body", slug = "test", name = "test", is_quote = F)
  posts <- get_img_files("test")
  ans <- airtable_post(slug, name = NULL, tab_name = "Testing")
  expect_equal(ans$fields$img_url, "https://www.gospelanalysis.com/img/test/test/plot.png")
  expect_true(str_detect(ans$fields$twitter_copy, fixed("#")))
  teardown()
})
