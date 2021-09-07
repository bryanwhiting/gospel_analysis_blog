# test_file('tests/testthat/test-utils.R')
library(glue)
root_img <- "/home/rstudio/gospel_analysis_blog/img"
test_dir <- glue("{root_img}/test")
teardown <- function() {unlink(test_dir, recursive = T)}

test_that("save_img() works for ggplot2", {
  p <- ggplot2::ggplot()
  save_img(p, 'test', 'test')

  path = glue("{root_img}/test/test/plot.png")
  expect_true(file.exists(file=path))
  teardown()
})

test_that('save_copy() works', {
  save_copy('test', 'test', 'hello world')
  path = glue("{root_img}/test/test/copy.txt")
  expect_true(file.exists(file=path))
  ans <- readLines(path)
  expect_equal(ans, "hello world")

  # Test that it doesn't break if something
  # already exists and properly overwrites
  save_copy('test', 'test', 'hello world2')
  ans <- readLines(path)
  expect_equal(ans, "hello world2")
  teardown()
})

test_that('copy() works',{
  x <- copy('hello world', "#hello")
  expect_equal(
    x,
    "hello world

#hello"
  )
})

test_that('read_copy() works', {
  save_copy('test', 'name', 'hello world\nhi')
  ans <- read_copy('test', 'name')
  expect_equal(ans, "hello world\nhi")
  teardown()
})

test_that("get_img_files() works", {
  p <- ggplot2::ggplot()
  save_img(p, 'test', 'test')
  save_copy('test', 'test', 'hello1')
  save_img(p, 'test', 'test2')
  save_copy('test', 'test2', 'hello2')
  ans <- get_img_files('test')
  expect_equal(length(ans), 2)
  expect_equal(ans$test$plot, "https://www.gospelanalysis.com/img/test/test/plot.png")
  expect_equal(ans$test2$copy, "hello2")
  teardown()
})
