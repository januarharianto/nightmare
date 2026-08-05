test_that("plans export creates one wide row per student with every plan group", {
  flat <- data.frame(
    student_id = c("2", "1", "1", "1", "1"),
    name = c("Zed Student", "Alex Student", "Alex Student", "Alex Student", "Alex Student"),
    sis_login_id = c("zstu0001", "astu0001", "astu0001", "astu0001", "astu0001"),
    group = c("Extensions", "Extensions", "Exam Accommodations", "Classroom Support", "Other"),
    display_detail = c(
      "Assignment Extension (3 days)",
      "Assignment Extension (1 week)",
      "Extra Working Time (15 min/hour)",
      "Laboratory Support",
      "Other detail"
    ),
    stringsAsFactors = FALSE
  )

  export <- build_plans_export(flat)

  expect_identical(
    names(export),
    c("Name", "Unikey", PLAN_GROUPS)
  )
  expect_equal(export$Name, c("Alex Student", "Zed Student"))
  expect_equal(export$Extensions[[1]], "Assignment Extension (1 week)")
  expect_equal(export$`Exam Accommodations`[[1]], "Extra Working Time (15 min/hour)")
  expect_equal(export$`Classroom Support`[[1]], "Laboratory Support")
  expect_equal(export$Other[[1]], "Other detail")
  expect_equal(export$`Exam Accommodations`[[2]], "")
})

test_that("plans export deduplicates repeated details and handles empty input", {
  flat <- data.frame(
    student_id = c("1", "1"),
    name = c("Alex Student", "Alex Student"),
    sis_login_id = c("astu0001", "astu0001"),
    group = c("Extensions", "Extensions"),
    display_detail = c("Assignment Extension (1 week)", "Assignment Extension (1 week)"),
    stringsAsFactors = FALSE
  )

  export <- build_plans_export(flat)
  expect_equal(export$Extensions, "Assignment Extension (1 week)")

  empty <- build_plans_export(flat[0, ])
  expect_equal(nrow(empty), 0)
  expect_identical(names(empty), names(export))
})

test_that("plans export appends groups not yet in the configured list", {
  flat <- data.frame(
    student_id = "1",
    name = "Alex Student",
    sis_login_id = "astu0001",
    group = "New Accommodation Group",
    display_detail = "New adjustment",
    stringsAsFactors = FALSE
  )

  export <- build_plans_export(flat)

  expect_identical(names(export), c("Name", "Unikey", PLAN_GROUPS, "New Accommodation Group"))
  expect_equal(export$`New Accommodation Group`, "New adjustment")
})
