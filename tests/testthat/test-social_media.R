# test_file('tests/testthat/test-social_media.R')
source(here::here("R/social_media.R"))
root_img <- here::here("img")
dir_t <- here::here("img/test")
dir_tt <- here::here("img/test/test")
slug <- "test"
name <- "test"

teardown <- function() {
  unlink(dir_t, recursive = T)
}

test_that("save_img() works for ggplot2", {
  p <- ggplot2::ggplot()
  save_img(p, slug, name, F)
  path <- fs::path(dir_tt, "plot.png")
  expect_true(file.exists(file = path))
  teardown()

  # quote
  save_img(p, slug, name, T)
  path <- fs::path(dir_tt, "plot.png")
  expect_true(file.exists(file = path))
  teardown()
})

test_that("save_img() works for gt::gt()", {
  p <- gt::gt(head(mtcars))
  save_img(p, slug, name)
  path <- fs::path(dir_tt, "plot.png")
  expect_true(file.exists(file = path))
  teardown()
})

test_that("write_content() and read_content() works", {
  write_content("hello world", slug, name, "content")
  path <- fs::path(dir_tt, "content.txt")
  expect_true(file.exists(file = path))
  ans <- read_content(slug, name, "content")
  expect_equal(ans, "hello world")

  # Test that it doesn't break if something
  # already exists and properly overwrites
  write_content("hello world2", slug, name, "content")
  ans <- read_content(slug, name, "content")
  expect_equal(ans, "hello world2")

  write_content("some body", slug, name, "body")
  ans <- read_content(slug, name, "body")
  expect_equal(ans, "some body")

  write_content("some body", slug, "dir1", "body")
  ans <- read_content(slug, "dir1", "body")
  expect_equal(ans, "some body")

  teardown()
})

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

test_that("make_post() works", {
  p <- ggplot2::ggplot()
  make_post(img = p, title = "title", body = "body", slug, name, is_quote = F)
  ans <- get_img_files(slug)
  expect_equal(ans$test$plot, "https://www.gospelanalysis.com/img/test/test/plot.png")
  expect_equal(ans$test$title, "title")
  expect_equal(ans$test$body, "body")
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
