# Test Dsciat function ####

test_that("Dsciat recognizes the class of the object", {
  data("raw_data")
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  iat_data <- iat_cleandata[[1]]
  expect_error(Dsciat(raw_data))
  expect_error(Dsciat(iat_data))
})


test_that("Dsciat recognizes the labels for Mapping A and Mapping B", {
  data("raw_data")
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("test.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"))
  sciat1 <- sciat_data[[1]]
  expect_error(Dsciat(sciat1,
                      mappingA = ".sc_dark.Darkbad",
                      mappingB = "test.sc_dark.Darkgood",
                      non_response = "alert"))
  expect_error(Dsciat(sciat1,
                      mappingA = "test.sc_dark.Darkbad",
                      mappingB = "test.sc_d.Darkgood",
                      non_response = "alert"))
  expect_error(Dsciat(sciat1,
                      mappingA = ".sc_dark.Darkbad",
                      mappingB = "test.sc_k.Darkgood",
                      non_response = "alert"))
  expect_error(Dsciat(sciat1,
                      mappingA = "test.sc_dark.Darkbad",
                      mappingB = "test.sc_dark.Darkbad",
                      non_response = "alert"))
})

test_that("Dsciat returns the right object", {
  data("raw_data")
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("test.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"))
  sciat1 <- sciat_data[[1]]
  sciat_score <- Dsciat(sciat1,
                        mappingA = "test.sc_dark.Darkbad",
                        mappingB = "test.sc_dark.Darkgood",
                        non_response = "alert")
  expect_equal(class(sciat_score)[2], "dsciat")
  expect_output(str(sciat_score))
})

