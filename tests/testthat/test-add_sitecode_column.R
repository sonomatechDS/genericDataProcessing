load('test_data/test_data.RData')

test_df1 <- annual_data %>% select(-aqs_sitecode)
test_df2 <- sample_data %>% select(-aqs_sitecode, -site_number)

test_that("Error thrown when cols not present", {
  expect_error(add_sitecode_column(test_df2))
})

test_that("Column added 1", {
  df1 <- add_sitecode_column(test_df1)
  expect_true('aqs_sitecode' %in% colnames(df1))
  expect_true(all(nchar(df1$aqs_sitecode) == 9))
  expect_true(all(c('state_code',
                    'county_code',
                    'site_number') %in% colnames(df1)))
})
