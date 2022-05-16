context("Hetu (personal identification number in Finland)")

test_that("hetu() works correctly", {
  expect_error(hetu("010101-0101", extract = ""))
  expect_error(hetu("010101-0101", extract = "", allow.temp = TRUE))
  expect_equal(hetu(c("010101-0101", "111111-111C"), extract = "sex"),
               c("Female", "Male"))
  expect_true(typeof(hetu(c("010101-0101", "111111-111C"), extract = NULL)) ==
                "list")
  #convert to character vector, check general format
  #this is intentionally wrong, no infix operator error
  expect_equal(hetu(010101-0101), NA)
  expect_true(is.na(hetu("010101-900R", extract = "sex")))
  expect_equal(hetu(c("111111-111C"), extract = "sex"), "Male")
  expect_equal(as.character(hetu("010101-0101")$hetu), "010101-0101")
  expect_equal(as.character(hetu(c("010101-0101"))$sex), "Female")
  expect_equal(hetu("010101-0101")$p.num, "010")
  expect_equal(as.character(hetu("010101-0101")$ctrl.char), "1")
  expect_equal(as.character(hetu("010101-0101")$date), "1901-01-01")
  expect_equal(hetu("010101-0101")$day, 1)
  expect_equal(hetu("010101-0101")$month, 1)
  expect_equal(hetu("010101-0101")$year, 1901)
  expect_false(hetu("320101-010A")$valid.pin) # check day
  expect_false(hetu("011301-010P")$valid.pin) # check month
  expect_false(suppressWarnings(hetu("0101-1-0101")$valid.pin)) # check year
  expect_false(hetu("010101B0101")$valid.pin) # check century marker
  expect_true(is.na(hetu("290201-010A")$date)) #Check if date exists
  expect_equal(as.character(hetu("010199+010A")$century), "+")
  expect_equal(as.character(hetu("010101-0101")$century), "-")
  expect_equal(as.character(hetu("010101A0101")$century), "A")
  #Checksum character validity (allowed characters)
  expect_false(hetu("010101-000G")$valid.pin)
  #Checksum character validity (correct character)
  expect_false(hetu("010101-0102")$valid.pin)
  expect_false(hetu("010101-000P", allow.temp = TRUE)$valid.pin)
  expect_false(hetu("010101-001R", allow.temp = TRUE)$valid.pin)
  expect_true(is.na(hetu("010101A900R")))
  expect_true(is.data.frame(hetu("010101A900R", allow.temp = TRUE)))
  #Test for 0 length vectors
  expect_true(is.na(hetu(c("010101A900R", "010101A900R"))))
  expect_false(hetu("010101-01013")$valid.pin)
  expect_true(!is.null(hetu("010101-0101", diagnostic = TRUE)))
})

test_that("pin_ctrl() works correctly", {
  expect_true(all(pin_ctrl(c("010101-0101", "111111-111C"))))
  expect_false(pin_ctrl("010101-010A"))
  expect_true(pin_ctrl("010101A900R", allow.temp = TRUE))
  expect_true(is.na(pin_ctrl("010101A900R", allow.temp = FALSE)))
})

test_that("bid_ctrl() works correctly", {
  expect_false(bid_ctrl("0737546-1"))
  expect_true(all(bid_ctrl(c("0737546-2", "1572860-0"))))
  expect_warning(bid_ctrl("0737546+A"))
  expect_false(bid_ctrl("3000001-1"))
  #this is intentionally wrong, no infix operator error
  expect_warning(bid_ctrl(0737546-1))
})
  
test_that("pin_to_date() works correctly and produces deprecation warning", {
  expect_warning(all((pin_to_date(c("010101-0101", "111111-111C")) ==
                        c("1901-01-01", "1911-11-11"))))
  expect_warning(all((pin_to_date(c("010101A0101", "111111A111C")) ==
                        c("2001-01-01", "2011-11-11"))))
})

test_that("pin_date() works correctly", {
  expect_true(all((pin_date(c("010101-0101", "111111-111C")) ==
                     c("1901-01-01", "1911-11-11"))))
  expect_true(all((pin_date(c("010101A0101", "111111A111C")) ==
                     c("2001-01-01", "2011-11-11"))))
})
 
test_that("pin_age() works correctly", {
  expect_true(pin_age("010101-0101", date = Sys.Date()) > 100)
  expect_true(all(pin_age(c("010101-0101", "111111-111C"),
                          date = Sys.Date()) > 100))
  expect_equal(pin_age(c("010101-0101", "111111-111C"),
                       date = c("2000-01-01", "2010-01-01")), c(99, 98))
  expect_error(pin_age(c("010101-0101"), date = c("2000-01-01", "2010-01-01")))
  expect_warning(pin_age("101010-101B", date = "1901-01-01"))
})

test_that("pin_sex() works correctly", {
  expect_equal(pin_sex("010101-0101"), "Female")
  expect_false(is.na(pin_sex("010101-0101")))
})

test_that("rpin() works correctly", {
  expect_equal(length(rpin(10)), 10)
  expect_equal(length(rpin(10, p.temp = 0.1)), 10)
  expect_equal(length(rpin(3,
                           start.date = "1895-01-01",
                           end.date = "1899-01-01")), 3)
})

test_that("rbid() works correctly", {
  expect_equal(length(rbid(3)), 3)
})

test_that("hetu_diagnostic works correctly", {
  expect_silent(hetu_diagnostic("010101-0101"))
  expect_error(hetu_diagnostic("010101-0101",
                               extract = "incorrect_diagnostic"))
  expect_false(hetu_diagnostic("010101-0102",
                               extract = "correct.ctrl.char")$correct.ctrl.char)
})

test_that("hetu_control_char works correctly", {
  expect_visible(hetu_control_char("010101-010", with.century = TRUE))
  expect_visible(hetu_control_char("010101010", with.century = FALSE))
  expect_visible(hetu_control_char(c("010101-010", "111111-111")))
  expect_error(hetu_control_char("010101-0101", with.century = TRUE))
  expect_error(hetu_control_char("010101B010", with.century = TRUE))
  expect_error(hetu_control_char("0101010101", with.century = FALSE))
})
