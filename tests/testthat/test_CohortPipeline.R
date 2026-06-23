library(data.table)

make_test_dt <- function() {
  data.table(
    id  = 1:10,
    age = c(17, 22, 35, NA, 41, 28, 19, 16, 67, 50),
    sex = c("F", "M", "F", "F", NA, "M", "M", "F", "F", "M"),
    grp = c("a", "a", "b", "b", "a", "b", "a", "b", "a", "b")
  )
}

test_that("constructor installs root cohort and copies user data", {
  d <- make_test_dt()
  cp <- CohortPipeline$new(d)

  expect_equal(cp$n_total(), 10L)
  expect_equal(cp$n_included("root"), 10L)

  # Mutating the user's dt does not change the pipeline's view.
  d[, age := age + 100]
  expect_equal(cp$get_included("root")$age[1], 17)
})

test_that("exclude_and_track records the log and removes rows", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$exclude_and_track("root", "Missing age", "is.na(age)")
  cp$exclude_and_track("root", "Under 18",    "age < 18")

  expect_equal(cp$n_included("root"), 6L)

  log <- cp$consort()
  expect_equal(nrow(log), 3L)
  expect_equal(log$reason, c("Missing sex", "Missing age", "Under 18"))
  expect_equal(log$n_excluded, c(1L, 1L, 2L))
  expect_equal(log$n_remaining, c(9L, 8L, 6L))
  expect_equal(log$expr_str, c("is.na(sex)", "is.na(age)", "age < 18"))
})

test_that("NA predicate values are treated as FALSE (rows kept)", {
  cp <- CohortPipeline$new(make_test_dt())
  # Predicate evaluates to NA on rows where age is NA; those rows survive.
  cp$exclude_and_track("root", "Strictly under 18", "age < 18")
  expect_true(4L %in% cp$get_included("root")$id) # row 4 has NA age
})

test_that("new_cohort creates an independent branch and freezes the parent", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")

  cp$new_cohort("females", from = "root")
  cp$exclude_and_track("females", "Not female", "sex != 'F'")

  # Parent unchanged by child operations
  expect_equal(cp$n_included("root"), 9L)
  # Child reflects further exclusion
  expect_equal(cp$n_included("females"), 5L)
  expect_setequal(cp$get_included("females")$sex, "F")

  # Multiple forks off the same parent are fine: the parent state stays
  # the same and both children snapshot the same definition.
  cp$new_cohort("males", from = "root")
  expect_equal(cp$n_included("males"), 9L)
})

test_that("freeze rule: branching prevents further exclusions on the parent", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$new_cohort("child", from = "root")

  expect_error(
    cp$exclude_and_track("root", "Under 18", "age < 18"),
    "frozen"
  )
})

test_that("freeze rule: setting an artifact prevents further exclusions", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$set_artifact("n", from = "root", fn = function(dt, sib) nrow(dt))

  expect_error(
    cp$exclude_and_track("root", "Under 18", "age < 18"),
    "frozen"
  )

  # Multiple artifacts on the same cohort are still allowed.
  cp$set_artifact("ids", from = "root", fn = function(dt, sib) dt$id)
  expect_equal(cp$get_artifact("root", "ids"), cp$get_included("root")$id)
})

test_that("freeze rule: list_cohorts reports the frozen flag", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$new_cohort("child", from = "root")

  cohorts <- cp$list_cohorts()
  expect_true("frozen" %in% names(cohorts))
  expect_true(cohorts[name == "root", frozen])
  expect_false(cohorts[name == "child", frozen])
})

test_that("get_included returns an independent copy", {
  cp <- CohortPipeline$new(make_test_dt())
  out <- cp$get_included("root")
  out[, age := age * 0]
  expect_equal(cp$get_included("root")$age[1], 17)
})

test_that("get_everyone reconstructs full per-row status", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$exclude_and_track("root", "Under 18",    "age < 18")

  ev <- cp$get_everyone("root")
  expect_equal(nrow(ev), 10L)
  expect_true(".cohort_status" %in% names(ev))
  expect_equal(sum(ev$.cohort_status == "included"), cp$n_included("root"))
  expect_true("Missing sex" %in% ev$.cohort_status)
  expect_true("Under 18" %in% ev$.cohort_status)

  # Branch view: child sees parent's exclusions plus its own.
  cp$new_cohort("adults", from = "root")
  cp$exclude_and_track("adults", "Female", "sex == 'F'")
  ev2 <- cp$get_everyone("adults")
  expect_equal(nrow(ev2), 10L)
  expect_true("Female" %in% ev2$.cohort_status)
  # Inherited reasons from parent are still visible on the child.
  expect_true("Missing sex" %in% ev2$.cohort_status)
})

test_that("set_artifact caches results and exposes siblings", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$set_artifact("n", from = "root",
    fn = function(dt, sib) nrow(dt))
  cp$set_artifact("groups", from = "root",
    fn = function(dt, sib) {
      stopifnot("n" %in% names(sib))
      sort(unique(dt$grp))
    })

  expect_equal(cp$get_artifact("root", "n"), 10L)
  expect_equal(cp$get_artifact("root", "groups"), c("a", "b"))
  expect_setequal(cp$list_artifacts("root"), c("n", "groups"))
})

test_that("set_artifact callbacks may freely mutate the dt argument", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$set_artifact("mutated_dt", from = "root",
    fn = function(dt, sib) {
      dt[, age := age * 2]   # mutate the supplied dt
      dt
    })
  # Pipeline's view is untouched
  expect_equal(cp$get_included("root")$age[1], 17)
  # Cached artifact saw the mutation
  expect_equal(cp$get_artifact("root", "mutated_dt")$age[1], 34)
})

test_that("schemas validate types, levels, and NA constraints", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$exclude_and_track("root", "Missing age", "is.na(age)")
  cp$declare_schema("root", schema = list(
    age = list(type = "numeric", na = FALSE),
    sex = list(type = "character", na = FALSE)
  ))
  expect_message(cp$validate(), "schemas passed")

  # Add a wrong-type spec
  cp$declare_schema("root", schema = list(
    age = list(type = "integer", na = FALSE)
  ), from = "root")
  expect_error(cp$validate(), "expected integer")
})

test_that("auto_validate raises on schema mismatch at the failure site", {
  cp <- CohortPipeline$new(make_test_dt(), auto_validate = TRUE)
  cp$declare_schema("root", schema = list(
    sex = list(type = "character", na = FALSE)  # NAs present!
  ))
  expect_error(cp$new_cohort("foo", from = "root"), "unexpected NAs")
})

test_that("list_cohorts and consort return tidy data.tables", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$new_cohort("females", from = "root")
  cp$exclude_and_track("females", "Not female", "sex != 'F'")

  cohorts <- cp$list_cohorts()
  expect_setequal(cohorts$name, c("root", "females"))
  expect_equal(cohorts[name == "females", parent], "root")
  expect_equal(cohorts[name == "root", n_own_steps], 1L)
  expect_equal(cohorts[name == "females", n_own_steps], 1L)

  log <- cp$consort()
  # Each branch should contribute only its OWN log entries.
  expect_equal(nrow(log), 2L)
  expect_setequal(log$branch, c("root", "females"))
})

test_that("error paths reject unknown branches and conflicting names", {
  cp <- CohortPipeline$new(make_test_dt())
  expect_error(cp$exclude_and_track("nope", "x", "TRUE"), "unknown branch")
  expect_error(cp$new_cohort("ok", from = "nope"),       "unknown parent")
  cp$new_cohort("ok", from = "root")
  # Same-parent re-issue is idempotent (required for cache replay).
  expect_silent(cp$new_cohort("ok", from = "root"))
  # Different parent on an existing name is a hard error.
  cp$new_cohort("other", from = "root")
  expect_error(cp$new_cohort("ok", from = "other"),      "already exists")
  expect_error(cp$get_artifact("root", "missing"),       "unknown artifact")
})

test_that("predicate length mismatch is reported clearly", {
  cp <- CohortPipeline$new(make_test_dt())
  expect_error(
    cp$exclude_and_track("root", "Bad", "TRUE"),  # length-1, not nrow
    "predicate returned length"
  )
})

test_that("exclude_and_track rejects non-logical predicate results", {
  cp <- CohortPipeline$new(make_test_dt())
  # Character / numeric / factor results would otherwise be silently coerced
  # by as.logical(), corrupting the exclusion counts.
  expect_error(cp$exclude_and_track("root", "char",   "sex"),
    "must return a logical vector, not character")
  expect_error(cp$exclude_and_track("root", "num",    "age"),
    "must return a logical vector, not numeric")
  expect_error(cp$exclude_and_track("root", "factor", "factor(sex)"),
    "must return a logical vector, not factor")
  # A logical *matrix* passes is.logical() but must still be rejected.
  expect_error(cp$exclude_and_track("root", "matrix", "cbind(age > 18, age > 40)"),
    "must return a logical vector, not matrix")
  # The failed calls leave the cohort untouched; a real logical predicate works.
  expect_equal(cp$n_included("root"), 10L)
  cp$exclude_and_track("root", "Under 18", "age < 18")
  expect_equal(cp$n_included("root"), 8L)
})

test_that("plot() defaults to every cohort regardless of freeze state", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")

  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(cp$plot())
  dev.off()

  # Adding an unfrozen leaf cohort -- it must still be included.
  cp$new_cohort("child", from = "root")
  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(cp$plot())
  dev.off()

  # Explicit cohort selection still works.
  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(cp$plot(cohorts = "child"))
  dev.off()

  # Unknown cohort is rejected.
  expect_error(cp$plot(cohorts = "missing"), "unknown cohort")
})



test_that("cache_file: warm replay preserves state, divergent ops recompute", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)

  # Cold run.
  cp <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$exclude_and_track("root", "Under 18",    "age < 18")
  cp$new_cohort("females", from = "root")
  cp$exclude_and_track("females", "Not female", "sex != 'F'")
  cp$set_artifact("dt_sub", from = "females",
    fn = function(dt, sib, argset) dt[, .(id, age, sex)],
    argset = list(version = 1L)
  )
  cp$save()
  expect_true(file.exists(cache))
  baseline <- list(
    root_n    = cp$n_included("root"),
    fem_n     = cp$n_included("females"),
    art_cols  = sort(names(cp$get_artifact("females", "dt_sub")))
  )

  # Warm replay with identical ops.
  cp2 <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp2$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp2$exclude_and_track("root", "Under 18",    "age < 18")
  cp2$new_cohort("females", from = "root")
  cp2$exclude_and_track("females", "Not female", "sex != 'F'")
  cp2$set_artifact("dt_sub", from = "females",
    fn = function(dt, sib, argset) dt[, .(id, age, sex)],
    argset = list(version = 1L)
  )
  expect_equal(cp2$n_included("root"),    baseline$root_n)
  expect_equal(cp2$n_included("females"), baseline$fem_n)
  expect_equal(sort(names(cp2$get_artifact("females", "dt_sub"))),
               baseline$art_cols)
})

test_that("cache_file: changed exclusion expr_str triggers re-execution + cascade", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)

  cp <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp$exclude_and_track("root", "Under 18", "age < 18")
  cp$new_cohort("females", from = "root")
  cp$exclude_and_track("females", "Not female", "sex != 'F'")
  cp$save()
  baseline_root <- cp$n_included("root")

  # Re-run with a stricter exclusion: age cutoff bumped to 21.
  cp2 <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp2$exclude_and_track("root", "Under 18", "age < 21")  # different expr_str
  cp2$new_cohort("females", from = "root")
  cp2$exclude_and_track("females", "Not female", "sex != 'F'")
  expect_lt(cp2$n_included("root"), baseline_root)  # fewer included now
  # The branched cohort cascade-invalidated and was rebuilt against
  # the new root state, so it exists with consistent state.
  expect_true("females" %in% cp2$list_cohorts()$name)
})

test_that("cache_file: changed argset re-runs the artifact fn", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)

  cp <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$set_artifact("subset", from = "root",
    fn = function(dt, sib, argset) dt[, argset$cols, with = FALSE],
    argset = list(cols = c("id", "age"))
  )
  expect_equal(sort(names(cp$get_artifact("root", "subset"))),
               c("age", "id"))
  cp$save()

  # Re-run with a different argset.
  cp2 <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp2$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp2$set_artifact("subset", from = "root",
    fn = function(dt, sib, argset) dt[, argset$cols, with = FALSE],
    argset = list(cols = c("id", "sex"))   # changed
  )
  expect_equal(sort(names(cp2$get_artifact("root", "subset"))),
               c("id", "sex"))
})

test_that("set_artifact accepts the legacy 2-arg fn signature", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$set_artifact("legacy", from = "root",
    fn = function(dt, sib) dt[, .(id, age)]
  )
  art <- cp$get_artifact("root", "legacy")
  expect_equal(sort(names(art)), c("age", "id"))
})

test_that("invalidate(cohort) drops the cohort and its descendants", {
  cp <- CohortPipeline$new(make_test_dt())
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$new_cohort("a", from = "root")
  cp$new_cohort("b", from = "a")
  expect_setequal(cp$list_cohorts()$name, c("root", "a", "b"))
  cp$invalidate("a")
  expect_setequal(cp$list_cohorts()$name, c("root"))
})

test_that("save errors when no cache_file is set", {
  cp <- CohortPipeline$new(make_test_dt())
  expect_error(cp$save(), "no cache_file set")
})

test_that("cache_file: version mismatch errors loudly", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)
  saveRDS(list(cache_version = 999L, base_dt = NULL,
               nodes = list(), schemas = list()), cache)
  expect_error(CohortPipeline$new(cache_file = cache), "cache file")
})

test_that("cache_file: dt content change is detected and rebuilds", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)

  cp <- CohortPipeline$new(make_test_dt(), cache_file = cache)
  cp$exclude_and_track("root", "Missing sex", "is.na(sex)")
  cp$save()

  # Same shape, different values: hash changes -> warn + rebuild.
  d2 <- make_test_dt()
  d2[, age := age + 1L]
  expect_warning(
    cp2 <- CohortPipeline$new(d2, cache_file = cache),
    "digest mismatch"
  )
  # After rebuild, root has no log entries (we have not re-issued any).
  expect_equal(nrow(cp2$consort()), 0L)
})

test_that("cache_file: a non-root cohort changing a later own exclusion drops the stale step", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)
  d <- data.table(id = 1:6, age = c(10,20,30,40,50,60), grp = c("a","a","b","b","c","c"))

  cp <- CohortPipeline$new(d, cache_file = cache)
  cp$exclude_and_track("root", "drop age<15", "age < 15")  # root own step (abs 1)
  cp$new_cohort("child", from = "root")                     # branched_at_log_len = 1
  cp$exclude_and_track("child", "drop grp a", "grp == 'a'") # child own #1 (abs 2)
  cp$exclude_and_track("child", "drop grp b", "grp == 'b'") # child own #2 (abs 3)
  cp$save()

  # Warm rerun: change the child's SECOND own exclusion (b -> c). The absolute
  # replay cursor must truncate the stale step rather than retaining it.
  cp2 <- CohortPipeline$new(d, cache_file = cache)
  cp2$exclude_and_track("root", "drop age<15", "age < 15")
  cp2$new_cohort("child", from = "root")
  cp2$exclude_and_track("child", "drop grp a", "grp == 'a'")
  cp2$exclude_and_track("child", "drop grp c", "grp == 'c'")

  steps <- cp2$consort()[branch == "child", reason]
  expect_equal(steps, c("drop grp a", "drop grp c"))  # no stale 'drop grp b'
  expect_equal(cp2$get_included("child")$id, c(3L, 4L))
})

test_that("cache_file: reordering new_cohort before a parent exclusion errors", {
  cache <- tempfile(fileext = ".rds")
  on.exit(unlink(cache), add = TRUE)
  d <- data.table(id = 1:6, age = c(10,20,30,40,50,60))

  cp <- CohortPipeline$new(d, cache_file = cache)
  cp$exclude_and_track("root", "under 18", "age < 18")  # excludes id 1
  cp$new_cohort("child", from = "root")
  cp$save()

  # Unchanged order replays cleanly -- the order guard must not false-positive.
  cp_ok <- CohortPipeline$new(d, cache_file = cache)
  cp_ok$exclude_and_track("root", "under 18", "age < 18")
  expect_silent(cp_ok$new_cohort("child", from = "root"))
  expect_equal(cp_ok$n_included("child"), 5L)

  # Reordered: branch before the parent exclusion. Cold this hits the freeze
  # rule; warm it used to silently keep a stale snapshot. Now it errors.
  cp_bad <- CohortPipeline$new(d, cache_file = cache)
  expect_error(cp_bad$new_cohort("child", from = "root"),
    "operation order changed")
})
