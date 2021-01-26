test_that("initialize test", {
  dir_path = "tmp/test"
  ttl_path = file.path(dir_path, "ttl")
  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  unlink(x = dir_path, recursive = TRUE)
  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  unlink(x = ttl_path, recursive = TRUE)
  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  mycache = RFastCache$new(dir=dir_path)
  expect_true(dir.exists(dir_path) && file.exists(ttl_path))

  unlink(x = dir_path, recursive = TRUE)
})

test_that("set, get, delete test", {
  dir_path = "tmp/test"
  ttl_path = file.path(dir_path, "ttl")
  mycache = RFastCache$new(dir=dir_path)

  first = c(1,2,3,4,5)
  second = mtcars
  third = plot(mtcars$mpg, mtcars$cyl)

  expect_true(mycache$get(key = "First", default = TRUE))
  mycache$set(key = "First", value = first, ttl = 1)
  expect_equal(mycache$get(key = "First"), first)

  Sys.sleep(1)
  expect_true(mycache$get(key = "First", default = TRUE))

  mycache$set(key = "First", value = second, ttl = 10)
  expect_equal(mycache$get(key = "First"), second)

  mycache$set(key = "first", value = first, ttl = 10)
  mycache$set(key = "second", value = second, ttl = 10)
  mycache$set(key = "third", value = third, ttl = 10)
  expect_equal(mycache$get(key = "first"), first)
  expect_equal(mycache$get(key = "second"), second)
  expect_equal(mycache$get(key = "third"), third)

  mycache$delete("second")
  expect_equal(mycache$get(key = "first"), first)
  expect_equal(mycache$get(key = "second"), NULL)
  expect_equal(mycache$get(key = "third"), third)

  unlink(x = dir_path, recursive = TRUE)
})


test_that("Test speed", {
  dir_path = "tmp/test"
  ttl_path = file.path(dir_path, "ttl")
  mycache = RFastCache$new(dir=dir_path)

  unlink(x = dir_path, recursive = TRUE)
})

