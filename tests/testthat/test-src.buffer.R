make_tmp_file <- function(text) {
  file <- tempfile()
  writeLines(text, file)  
  
  file
}

context("Source buffer construction")


test_that("source buffer can be constructed from a string", {
  expect_s3_class(src.buffer(text = "Some code"), "src.buffer.location")
  expect_s3_class(src.buffer(text = c("First line", "second line")), "src.buffer.location")
})

test_that("source buffer can be constructed from a file", {
  expect_s3_class(src.buffer(file = make_tmp_file("Some code")), "src.buffer.location")
  expect_s3_class(src.buffer(text = make_tmp_file(c("First line", "second line"))), "src.buffer.location")
})


context("Source buffer consumption")

test_that("basic reads on a source buffer are correct", {
  c0 <- src.buffer(text = "Some code")
  
  out <- src.buffer.read(c0, nchar("Some"))
  
  expect_identical(out$value, "Some")
  expect_identical(out$location$row, 1)
  expect_identical(out$location$col, nchar("Some")+1)
  expect_identical(out$location$line, " code")
})

test_that("new line reads on a source buffer are correct", {
  c0 <- src.buffer(text = c("", "Some code"))
  
  out <- src.buffer.read(c0, 1)
  
  expect_identical(out$value, "\n")
  expect_s3_class(out$location, "src.buffer.location")
  
  expect_identical(out$location$row, 2)
  expect_identical(out$location$col, 1)
  expect_identical(out$location$line, "Some code")
})

test_that("Sequential reads on a source buffer are correct", {
  location <- src.buffer(text = c("First line", "Second line"))
  
  out <- src.buffer.read(location, nchar("First"))
  expect_identical(out$value, "First")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location
  
  out <- src.buffer.read(location, nchar(" "))
  expect_identical(out$value, " ")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location

  out <- src.buffer.read(location, nchar("line"))
  expect_identical(out$value, "line")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location

  out <- src.buffer.read(location, 1)
  expect_identical(out$value, "\n")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location

  out <- src.buffer.read(location, nchar("Second"))
  expect_identical(out$value, "Second")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location
  
  out <- src.buffer.read(location, nchar(" "))
  expect_identical(out$value, " ")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location

  out <- src.buffer.read(location, nchar("line"))
  expect_identical(out$value, "line")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location

  out <- src.buffer.read(location, 1)
  expect_identical(out$value, "\n")
  expect_s3_class(out$location, "src.buffer.location")
  location <- out$location
  
  expect_true(location == src.buffer.eof)
})



context("Source buffer location ordering construction")

test_that("ordering relations between two different locations on the same line are correct", {
  c0 <- src.buffer(text = "Some code")
  c1 <- src.buffer.read(c0, 5)$location
  
  expect_true(c0$row == c1$row)

  expect_true(c0 < c1)
  expect_true(c0 <= c1)
  expect_true(c0 != c1)
  expect_true(c1 > c0)
  expect_true(c1 >= c0)


  expect_false(c1 < c0)
  expect_false(c1 <= c0)
  expect_false(c0 > c1)
  expect_false(c0 >= c1)
  expect_false(c0 == c1)
})

test_that("ordering relations between different locations on different lines are correct", {
  c0 <- src.buffer(text = c("Some","source","code"))
  c1 <- src.buffer.read(src.buffer.read(c0, nchar("Some"))$location, 1)$location
  
  expect_true(c1$row > c0$row)

  expect_true(c0 < c1)
  expect_true(c0 <= c1)
  expect_true(c0 != c1)
  expect_true(c1 > c0)
  expect_true(c1 >= c0)


  expect_false(c1 < c0)
  expect_false(c1 <= c0)
  expect_false(c0 > c1)
  expect_false(c0 >= c1)
  expect_false(c0 == c1)
})

test_that("ordering relation is symmetric", {
  c0 <- src.buffer(text = "Some code")
  
  expect_true(c0 <= c0)
  expect_true(c0 >= c0)
  expect_true(c0 == c0)

  expect_false(c0 < c0)
  expect_false(c0 > c0)
  expect_false(c0 != c0)
})

test_that("ordering relations with the eof location are correct", {
  c0 <- src.buffer(text = "Code")
  c1 <- src.buffer.read(src.buffer.read(c0, nchar("Code"))$location, 1)$location
  
  expect_true(c0 < src.buffer.eof)
  expect_true(c0 <= src.buffer.eof)
  expect_true(c0 != src.buffer.eof)
  expect_true(src.buffer.eof > c0)
  expect_true(src.buffer.eof >= c0)

  expect_false(src.buffer.eof < c0)
  expect_false(src.buffer.eof <= c0)
  expect_false(c0 > src.buffer.eof)
  expect_false(c0 >= src.buffer.eof)
  expect_false(c0 == src.buffer.eof)
  
  expect_true(c1 == src.buffer.eof)
  expect_true(c1 <= src.buffer.eof)
  expect_true(c1 >= src.buffer.eof)


  expect_false(c1 > src.buffer.eof)
  expect_false(c1 < src.buffer.eof)
  expect_false(c1 != src.buffer.eof)
})




# devtools::document()
# devtools::load_all()
#
#
# buff <- src.buffer(text = c("my little", "banana"))
#
# #buff
#
# buff == src.buffer.eof
#
# buff
#
#
#
#
#
# x <- src.buffer.read(buff, 2)
#
#
#
#
# buff == x$location
#
# buff %to% x$location
#
# buff <- x$location
#
# x <- buff %to% src.buffer.read(buff, 3)$location
#
# as.data.frame(x)
#
# writeLines(format(x))
#
#
# x
#
# stop()
#
#
# ff <- function(x, y) {
#   print(missing(x))
#   print(missing(y))
# }
#
#
# ff("hello")
#
#
# stop()
# devtools::document()
# devtools::load_all()
#
#
# input <- source.text("Hello my puppy")
#
# input
#
# #library(crayon)
#
# blue <- crayon::blue
# green <- crayon::green
#
# print(blue $ green)
#
# panic("Oh my god!")
#
