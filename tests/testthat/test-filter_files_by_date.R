file_list <- c("12345_20160101_20160115_sample.csv",
               "12345_20160116_20160131_sample.csv",
               "12345_20160201_20160215_sample.csv",
               "12345_20160216_20160228_sample.csv",
               "12345_20160301_20160315_sample.csv",
               "12345_20160316_20160331_sample.csv",
               "12345_20170101_20170115_sample.csv",
               "12345_20170116_20170131_sample.csv",
               "12345_20180101_20180115_sample.csv",
               "12345_20180116_20180131_sample.csv",
               "12345_20190101_20190115_sample.csv",
               "12345_20190116_20190131_sample.csv",
               "12345_20200101_20200115_sample.csv",
               "12345_20200116_20200131_sample.csv"
               )

test_that("start year works", {
  expected <- c("12345_20180101_20180115_sample.csv",
                "12345_20180116_20180131_sample.csv",
                "12345_20190101_20190115_sample.csv",
                "12345_20190116_20190131_sample.csv",
                "12345_20200101_20200115_sample.csv",
                "12345_20200116_20200131_sample.csv")

  l1 <- filter_files_by_date('2018', file_list)
  expect_equal(l1, expected)
})

test_that("year range works", {
  expected <- c("12345_20170101_20170115_sample.csv",
               "12345_20170116_20170131_sample.csv",
               "12345_20180101_20180115_sample.csv",
               "12345_20180116_20180131_sample.csv",
               "12345_20190101_20190115_sample.csv",
               "12345_20190116_20190131_sample.csv")
  l2 <- filter_files_by_date(c('2017','2019'), file_list)
  expect_equal(l2, expected)
})

test_that("date works", {
  expected <- c("12345_20160216_20160228_sample.csv",
                "12345_20160301_20160315_sample.csv",
                "12345_20160316_20160331_sample.csv",
                "12345_20170101_20170115_sample.csv",
                "12345_20170116_20170131_sample.csv",
                "12345_20180101_20180115_sample.csv",
                "12345_20180116_20180131_sample.csv",
                "12345_20190101_20190115_sample.csv",
                "12345_20190116_20190131_sample.csv",
                "12345_20200101_20200115_sample.csv",
                "12345_20200116_20200131_sample.csv")

  l3 <- filter_files_by_date('20160217', file_list)
  expect_equal(l3, expected)
})

test_that("date range works", {
  expected <- c("12345_20160316_20160331_sample.csv",
                "12345_20170101_20170115_sample.csv",
                "12345_20170116_20170131_sample.csv",
                "12345_20180101_20180115_sample.csv",
                "12345_20180116_20180131_sample.csv")

  l4 <- filter_files_by_date(c('20160325', '20181031'), file_list)
  expect_equal(l4, expected)
})
