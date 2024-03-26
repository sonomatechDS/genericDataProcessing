test_that("clean_monitors", {
  m <- fread('test_data/aqs_monitors.csv')
  m <- clean_codes(m %>% janitor::clean_names())
  m_non_tribal <- m %>% filter(is.na(tribal_code))
  m_tribal <- m %>% filter(!is.na(tribal_code))
  expect_true(all(nchar(m_tribal$site_code) == 3))
  expect_true(all(nchar(m_non_tribal$site_code) == 4))
})
