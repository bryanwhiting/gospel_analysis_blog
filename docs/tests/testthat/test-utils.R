# test_file('tests/testthat/test-utils.R')
source("/home/rstudio/gospel_analysis_blog/R/utils.R")
library(glue)
root_img <- "/home/rstudio/gospel_analysis_blog/img"
test_dir <- glue("{root_img}/test")
teardown <- function() {
  unlink(test_dir, recursive = T)
}

test_that("save_img() works for ggplot2", {
  p <- ggplot2::ggplot()
  save_img(p, "test", "test")

  path <- glue("{root_img}/test/test/plot.png")
  expect_true(file.exists(file = path))
  teardown()
})

test_that("save_copy() works", {
  save_copy("hello world", "test", "test")
  path <- glue("{root_img}/test/test/copy.txt")
  expect_true(file.exists(file = path))
  ans <- readLines(path)
  expect_equal(ans, "hello world")

  # Test that it doesn't break if something
  # already exists and properly overwrites
  save_copy("hello world2", "test", "test")
  ans <- readLines(path)
  expect_equal(ans, "hello world2")
  teardown()
})

test_that("copy() works", {
  x <- copy("hello world", "#hello")
  expect_equal(
    x,
    "hello world

#hello"
  )
})

test_that("read_copy() works", {
  save_copy("hello world\nhi", "test", "name")
  ans <- read_copy("test", "name")
  expect_equal(ans, "hello world\nhi")
  teardown()
})

test_that("get_img_files() works", {
  p <- ggplot2::ggplot()
  save_img(p, "test", "test")
  save_copy("hello1", "test", "test")
  save_img(p, "test", "test2")
  save_copy("hello2", "test", "test2")
  ans <- get_img_files("test")
  expect_equal(length(ans), 2)
  expect_equal(ans$test$plot, "https://www.gospelanalysis.com/img/test/test/plot.png")
  expect_equal(ans$test2$copy, "hello2")
  teardown()
})

test_that("make_post() works", {
  p <- ggplot2::ggplot()
  make_post(plot = p, title = "title", body = "body", slug = "test", name = "test", is_quote = F)
  ans <- get_img_files('test')
  expect_equal(ans$test$plot, "https://www.gospelanalysis.com/img/test/test/plot.png")
  expect_equal(ans$test$title, "title")
  expect_equal(ans$test$body, "body")
  teardown()
})

test_that("airtable_post() works", {
  p <- ggplot2::ggplot()
  photopost(p, caption = "hello", slug = "test", name = "test", hashtag = "#hello")
  posts <- get_img_files("test")
  ans <- airtable_post("test", tab_name = "Testing")
  expect_equal(ans$fields$attachment_url, "https://www.gospelanalysis.com/img/test/test/plot.png")
})
