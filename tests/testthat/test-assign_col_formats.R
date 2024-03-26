

test_that('sample columns assigned correctly', {
  files_to_import = list.files('test_data/raw_data/', pattern = 'sample')
  path = 'test_data/raw_data/'

  col_formats <- assign_col_formats(path, files_to_import)

  expect_equivalent(col_formats$pad2_cols, c('STATE_CODE'))
  expect_equivalent(col_formats$pad3_cols, c('METHOD_CODE', 'COUNTY_CODE'))
  expect_equivalent(col_formats$pad4_cols, c('SITE_NUMBER'))
  expect_equivalent(col_formats$pad9_cols, character(0))
  expect_equivalent(col_formats$date_cols, c('DATE_LOCAL'))
})

test_that('daily columns assigned correctly', {
  files_to_import = list.files('test_data/raw_data/', pattern = 'daily')
  path = 'test_data/raw_data/'

  col_formats <- assign_col_formats(path, files_to_import)

  expect_equivalent(col_formats$pad2_cols, c('STATE_CODE'))
  expect_equivalent(col_formats$pad3_cols, c('METHOD_CODE', 'COUNTY_CODE'))
  expect_equivalent(col_formats$pad4_cols, c('SITE_NUMBER'))
  expect_equivalent(col_formats$pad9_cols, character(0))
  expect_equivalent(col_formats$date_cols, c('DATE_LOCAL'))
})

test_that('annual columns assigned correctly', {
  files_to_import = list.files('test_data/raw_data/', pattern = 'annual.*csv')
  path = 'test_data/raw_data/'

  col_formats <- assign_col_formats(path, files_to_import)

  expect_equivalent(col_formats$pad2_cols, c('STATE_CODE'))
  expect_equivalent(col_formats$pad3_cols, c('COUNTY_CODE'))
  expect_equivalent(col_formats$pad4_cols, c('SITE_NUMBER'))
  expect_equivalent(col_formats$pad9_cols, character(0))
  expect_equivalent(col_formats$date_cols, character(0))
})


test_that('Collo columns assigned correctly', {
  files_to_import = list.files('test_data/raw_data/', pattern = 'Collo')
  path = 'test_data/raw_data/'

  col_formats <- assign_col_formats(path, files_to_import)

  expect_equivalent(col_formats$pad2_cols, c('STATE_CODE'))
  expect_equivalent(col_formats$pad3_cols, c("PRIMARY_METHOD_CODE",
                                            "COLLOCTATED_METHOD_CODE",
                                            'COUNTY_CODE'))
  expect_equivalent(col_formats$pad4_cols, c("SITE_NUMBER"))
                    # ,
                    #                          "PQAO_CODE",
                    #                          "MONITORING_AGENCY_CODE",
                    #                          "ANALYZING_AGENCY_CODE",
                    #                          "AUDITING_AGENCY_CODE"))
  expect_equivalent(col_formats$pad9_cols, character(0))
  expect_equivalent(col_formats$date_cols, c('ASSESSMENT_DATE'))
})

test_that('agencies columns assigned correctly', {
  files_to_import = list.files('test_data/', pattern = 'agencies')
  path = 'test_data/'

  col_formats <- assign_col_formats(path, files_to_import)

  expect_equivalent(col_formats$pad2_cols, character(0))
  expect_equivalent(col_formats$pad3_cols, character(0))
  #expect_equivalent(col_formats$pad4_cols, c("Agency Code"))
  expect_equivalent(col_formats$pad9_cols, character(0))
  expect_equivalent(col_formats$date_cols, character(0))
})
