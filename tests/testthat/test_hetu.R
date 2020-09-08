context("Hetu (personal identification number in Finland")

test_that("hetu() works correctly", {
  expect_error(hetu("010101-0101", extract = ""))
  expect_equal(hetu(c("010101-0101", "111111-111C"), extract = "sex"), c("Female", "Male"))
  expect_true(typeof(hetu(c("010101-0101", "111111-111C"), extract = NULL)) == "list")
  expect_warning(hetu(010101-0101)) #convert to character vector, check general format
  expect_true(is.na(hetu("010101-900R", extract = "sex")))
  expect_equal(hetu(c("111111-111C"), extract = "sex"), "Male")
  expect_equal(as.character(hetu("010101-0101")$hetu), "010101-0101")
  expect_equal(as.character(hetu(c("010101-0101"))$sex), "Female")
  expect_equal(hetu("010101-0101")$personal.number, "010")
  expect_equal(as.character(hetu("010101-0101")$checksum), "1")
  expect_equal(as.character(hetu("010101-0101")$date), "1901-01-01")
  expect_equal(hetu("010101-0101")$day, 1)
  expect_equal(hetu("010101-0101")$month, 1)
  expect_equal(hetu("010101-0101")$year, 1901)
  expect_warning(hetu("320101-010A")) #Check day warning
  expect_warning(hetu("011301-010P")) #Check month warning
  expect_true(is.na(hetu("0101-1-0101"))) #Check year
  expect_warning(hetu("010101B0101")) #Check century
  expect_true(is.na(hetu("290201-010A"))) #Check if date exists
  expect_equal(as.character(hetu("010199+010A")$century), "+")
  expect_equal(as.character(hetu("010101-0101")$century), "-")
  expect_equal(as.character(hetu("010101A0101")$century), "A")
  expect_warning(hetu("010101-000G")) #Checksum character validity (allowed characters)
  expect_warning(hetu("010101-000P", allow.temp = TRUE)) #Check personal identification number
  expect_warning(hetu("010101-001R", allow.temp = TRUE)) #Check personal identification number
  expect_warning(hetu("010101-0102")) #Checksum character validity (correct character)
  expect_true(!is.null(hetu("010101A900R", allow.temp = TRUE)))
  expect_error(hetu(c("010101A900R", "010101A900R"))) #Test for 0 length vectors
  expect_warning(hetu("010101-01013"))
})

test_that("pin_ctrl() works correctly", {
  expect_true(all(pin_ctrl(c("010101-0101", "111111-111C"))))
  expect_false(pin_ctrl("010101-010A"))
  expect_true(pin_ctrl("010101A900R", allow.temp = TRUE))
  expect_false(pin_ctrl("010101A900R", allow.temp = FALSE))
})
  
test_that("pin_to_date() works correctly", {
  expect_warning(all((pin_to_date(c("010101-0101", "111111-111C")) == c("1901-01-01", "1911-11-11"))))
  expect_warning(all((pin_to_date(c("010101A0101", "111111A111C")) == c("2001-01-01", "2011-11-11"))))
})

test_that("pin_date() works correctly", {
  expect_true(all((pin_date(c("010101-0101", "111111-111C")) == c("1901-01-01", "1911-11-11"))))
  expect_true(all((pin_date(c("010101A0101", "111111A111C")) == c("2001-01-01", "2011-11-11"))))
})
 
test_that("pin_age() works correctly", { 
  expect_true(pin_age("010101-0101", date = Sys.Date()) > 100)
  expect_true(all(pin_age(c("010101-0101", "111111-111C"), date = Sys.Date()) > 100))
  expect_equal(pin_age(c("010101-0101", "111111-111C"), date = c("2000-01-01", "2010-01-01")), c(99,98))
  expect_error(pin_age(c("010101-0101"), date = c("2000-01-01", "2010-01-01")))
  expect_warning(pin_age("101010-101B", date = "1901-01-01"))
})
 
test_that("pin_sex() works correctly", { 
  expect_true(is.na(pin_sex("010101-010A")))
  expect_false(is.na(pin_sex("010101-0101")))
})

test_that("rpin() works correctly", {
  expect_equal(length(rpin(100)), 100)
  expect_equal(length(rpin(100, p.temp = 0.1)), 100)
  
})