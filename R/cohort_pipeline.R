#' R6 Class for Cohort Construction with Provenance
#'
#' @description
#' `CohortPipeline` builds analytic cohorts as a tree of named branches with
#' full exclusion provenance. Each branch derives from a parent branch by
#' applying a sequence of named exclusion rules. Every exclusion is
#' recorded -- its reason, the predicate that produced it, the number of
#' subjects affected -- so the resulting object can drive a CONSORT diagram
#' and serve as the auditable record of how the analytic dataset was
#' constructed.
#'
#' Cohort construction is kept strictly upstream of analysis: the class
#' produces analytic data tables that downstream code can consume. See
#' `vignette("cohort", package = "cohort")` for a worked example.
#'
#' @section Storage strategy:
#' A `CohortPipeline` stores a single shared base data table and, for each
#' branch, a small per-row integer status vector identifying which rows are
#' included and which step excluded them. Branching is therefore O(n) in
#' the number of rows of the base table and never copies the data values,
#' so deep cohort trees stay flat in memory.
#'
#' @section Freeze rule:
#' A cohort becomes **frozen** the first time either:
#'
#' 1. another cohort branches from it (via `$new_cohort(from = X)`), or
#' 2. an artifact is set on it (via `$set_artifact(from = X)`).
#'
#' After freezing, `$exclude_and_track()` on that cohort errors. The rule
#' guarantees that a cohort's name maps to exactly one definition forever:
#' once children depend on it, its exclusion list is fixed, and any
#' cached artifact stays consistent with the included rows that produced
#' it. The practical workflow is "apply all exclusions on a cohort, then
#' branch from it or attach artifacts." Multi-way forks are unaffected:
#' you can branch a frozen cohort as many times as you like.
#'
#' @section Mutation contract:
#' - `CohortPipeline$new(dt)` makes a defensive copy of `dt` once. The
#'   user's data table is never modified.
#' - `$get_included(cohort)` returns an independent copy. The caller may
#'   mutate it freely without affecting any other cohort or the shared
#'   base table.
#' - The data table passed to a `$set_artifact()` callback is always an
#'   independent copy. Callbacks may mutate it freely.
#' - `$get_everyone(cohort)` returns an independent copy with a
#'   `.cohort_status` column reconstructed from the branch's status
#'   vector.
#'
#' @section Public API:
#' - `CohortPipeline$new(dt, cache_file, label)` -- construct a pipeline
#'   with a shared base table installed as the root cohort. With
#'   `cache_file`, restore from a prior run if the file exists.
#' - `$new_cohort(name, from, label)` -- branch from an existing cohort.
#' - `$exclude_and_track(branch, reason, expr_str)` -- apply a string-form
#'   predicate and log the exclusion.
#' - `$set_artifact(name, from, fn, argset)` -- cache a derived object on a
#'   cohort. `fn` may be `function(dt, sib)` or `function(dt, sib, argset)`.
#' - `$get_included(cohort)` -- included rows of a cohort.
#' - `$get_everyone(cohort)` -- full-cohort view with a reconstructed
#'   `.cohort_status` column.
#' - `$get_artifact(cohort, name)` -- retrieve a cached artifact.
#' - `$n_included(cohort)`, `$n_total()` -- row counts.
#' - `$list_cohorts()`, `$list_artifacts(cohort)`, `$list_schemas()`
#' - `$declare_schema(branch, schema, from)`, `$validate()` -- column
#'   contracts.
#' - `$consort()` -- long-form exclusion log across all branches.
#' - `$draw_consort_panels(panels, file)` -- render CONSORT diagrams.
#' - `$save(file)`, `$invalidate(cohort, artifact)` -- incremental cache
#'   persistence and manual cache invalidation.
#' - `$print()` -- concise text summary of the cohort tree.
#'
#' @section Predicate strings:
#' Exclusion predicates are passed as strings (`expr_str`) and parsed with
#' `parse(text = expr_str)`. The string is evaluated against the included
#' subset of the base table, so predicates may safely assume that earlier
#' exclusions have already removed invalid rows. `NA` predicate results
#' are treated as `FALSE` (rows are kept). The original string is stored
#' verbatim in the exclusion log, which keeps cohort definitions
#' serializable and auditable.
#'
#' @examples
#' library(data.table)
#' d <- data.table(
#'   id  = 1:10,
#'   age = c(17, 22, 35, NA, 41, 28, 19, 16, 67, 50),
#'   sex = c("F", "M", "F", "F", NA, "M", "M", "F", "F", "M")
#' )
#'
#' cp <- CohortPipeline$new(d)
#'
#' # Root-level exclusions on the shared base
#' cp$exclude_and_track("root", "Missing sex",     "is.na(sex)")
#' cp$exclude_and_track("root", "Missing age",     "is.na(age)")
#' cp$exclude_and_track("root", "Under 18",        "age < 18")
#'
#' # Branch into an "adults_female" cohort
#' cp$new_cohort("adults_female", from = "root")
#' cp$exclude_and_track("adults_female", "Not female", "sex != 'F'")
#'
#' # Cache a derived artifact on the cohort
#' cp$set_artifact("mean_age", from = "adults_female",
#'   fn = function(dt, sib) mean(dt$age))
#'
#' cp$list_cohorts()
#' cp$consort()
#' cp$get_artifact("adults_female", "mean_age")
#'
#' @import data.table
#' @import R6
#' @importFrom digest digest
#' @export
CohortPipeline <- R6::R6Class(
  "CohortPipeline",
  cloneable = FALSE,
  public = list(

    #' @description
    #' Create a new `CohortPipeline`. If `cache_file` is set and the file
    #' exists, the pipeline is restored from that snapshot and `dt` is
    #' used only as a sanity check (its dimensions and column names must
    #' match the cached base table). Otherwise `dt` is installed as the
    #' root cohort.
    #' @param dt A `data.table` to install as the root cohort. Required
    #'   on cold construction; optional on warm cache load.
    #' @param label Optional character. Display label for the root cohort
    #'   (used in CONSORT diagrams and `list_cohorts()`). Defaults to
    #'   `"Cohort participants"`. Refreshed silently on warm cache load
    #'   so changing the label between runs is allowed.
    #' @param cache_file Optional character path. When supplied, an
    #'   incremental cache is enabled. If the file exists, the pipeline
    #'   is restored from it and subsequent operations replay the
    #'   recorded log on cache hits, recomputing only divergent steps.
    #'   If the file does not exist, fresh state is built and `$save()`
    #'   writes to this path. Recommended idiom for scripts:
    #'   `on.exit(cp$save(), add = TRUE)` near the top.
    #' @param auto_validate Logical. When `TRUE`, `$validate()` is invoked
    #'   automatically after every `$new_cohort()` and `$set_artifact()`
    #'   call so schema mismatches stop at the failure site rather than
    #'   accumulating until the next manual `$validate()`. Defaults to
    #'   `FALSE`.
    #' @return A new `CohortPipeline` instance.
    initialize = function(dt = NULL, cache_file = NULL,
                          label = NULL, auto_validate = FALSE) {
      private$schemas <- list()
      private$nodes <- list()
      private$auto_validate <- isTRUE(auto_validate)
      private$cache_file <- cache_file

      if (!is.null(cache_file) && file.exists(cache_file)) {
        snap <- readRDS(cache_file)
        if (!identical(snap$cache_version, .COHORT_CACHE_VERSION)) {
          stop("CohortPipeline: cache file '", cache_file, "' is version ",
            snap$cache_version, " but this package expects ",
            .COHORT_CACHE_VERSION, ". Delete the cache to start fresh.",
            call. = FALSE)
        }
        # Verify the supplied data matches the cached base table via a
        # full content hash. Cheap (sub-second on millions of rows) and
        # catches silent data updates that would otherwise produce
        # wrong results from stale cached state.
        cache_match <- TRUE
        dt_hash <- NULL
        if (!is.null(dt)) {
          dt_hash <- digest::digest(dt, algo = "spookyhash")
          cache_match <- identical(dt_hash, snap$base_dt_hash)
        }
        if (cache_match) {
          private$base_dt      <- snap$base_dt
          private$base_dt_hash <- snap$base_dt_hash
          private$nodes        <- snap$nodes
          private$schemas      <- snap$schemas
          for (nm in names(private$nodes)) {
            private$nodes[[nm]]$replay_cursor <-
              private$nodes[[nm]]$branched_at_log_len
          }
          # Refresh root label if the user explicitly passed one.
          # Labels are presentation, not part of the cohort identity.
          if (!is.null(label) && "root" %in% names(private$nodes)) {
            private$nodes$root$label <- label
          }
        } else {
          warning("CohortPipeline: supplied 'dt' content does not match ",
            "the cached base table (digest mismatch); discarding cache ",
            "and rebuilding from scratch.")
          private$install_base(dt, label = label %||% "Cohort participants")
          private$base_dt_hash <- dt_hash
        }
      } else if (!is.null(dt)) {
        private$install_base(dt, label = label %||% "Cohort participants")
        private$base_dt_hash <- digest::digest(dt, algo = "spookyhash")
      }
    },

    #' @description
    #' Declare a column-type / level / NA contract for a branch. Validation
    #' runs only when `$validate()` is called (or automatically when the
    #' pipeline was constructed with `auto_validate = TRUE`).
    #' @param branch Character. Branch name to attach the schema to.
    #' @param schema Named list. Each element describes one column with
    #'   fields:
    #'   - `type`: one of `"integer"`, `"numeric"`, `"factor"`, `"logical"`,
    #'     `"Date"`, `"character"`.
    #'   - `levels` (factor only): expected `levels()` vector.
    #'   - `na`: if `FALSE`, the column must contain no `NA`s.
    #' @param from Optional character. If supplied, the new schema starts
    #'   as a copy of the schema attached to `from` and the entries in
    #'   `schema` are merged on top.
    #' @return The pipeline (invisibly).
    declare_schema = function(branch, schema = NULL, from = NULL) {
      if (!is.null(from)) {
        if (!from %in% names(private$schemas)) {
          stop("declare_schema: unknown 'from' branch: ", from, call. = FALSE)
        }
        base <- private$schemas[[from]]
        if (!is.null(schema)) {
          for (nm in names(schema)) base[[nm]] <- schema[[nm]]
        }
        private$schemas[[branch]] <- base
      } else {
        if (is.null(schema)) {
          stop("declare_schema: 'schema' must be supplied when 'from' is NULL.",
            call. = FALSE)
        }
        private$schemas[[branch]] <- schema
      }
      invisible(self)
    },

    #' @description
    #' Validate every declared schema against the included rows of its
    #' branch. Throws an error listing every mismatch found.
    #' @return The pipeline (invisibly), if validation passes.
    validate = function() {
      errors <- character()
      for (branch in names(private$schemas)) {
        schema <- private$schemas[[branch]]
        if (!branch %in% names(private$nodes)) {
          errors <- c(errors, sprintf("[%s] Branch not found", branch))
          next
        }
        dt <- self$get_included(branch)
        for (col_name in names(schema)) {
          spec <- schema[[col_name]]
          if (!col_name %in% names(dt)) {
            errors <- c(errors,
              sprintf("[%s] Missing column: %s", branch, col_name))
            next
          }
          col <- dt[[col_name]]
          ok <- switch(spec$type,
            "integer"   = is.integer(col),
            "numeric"   = is.numeric(col),
            "factor"    = is.factor(col),
            "logical"   = is.logical(col),
            "Date"      = inherits(col, "Date"),
            "character" = is.character(col),
            TRUE
          )
          if (!ok) {
            errors <- c(errors, sprintf("[%s] %s: expected %s, got %s",
              branch, col_name, spec$type, class(col)[1]))
          }
          if (identical(spec$type, "factor") && !is.null(spec$levels) &&
              !identical(levels(col), spec$levels)) {
            errors <- c(errors,
              sprintf("[%s] %s: factor levels mismatch", branch, col_name))
          }
          if (isFALSE(spec$na) && anyNA(col)) {
            errors <- c(errors,
              sprintf("[%s] %s: unexpected NAs (%d)",
                branch, col_name, sum(is.na(col))))
          }
        }
      }
      if (length(errors) > 0L) {
        stop("CohortPipeline validation failed:\n  ",
          paste(errors, collapse = "\n  "), call. = FALSE)
      }
      message("[validate] All CohortPipeline schemas passed")
      invisible(self)
    },

    #' @description
    #' Tabulate the names and column counts of all declared schemas.
    #' @return A `data.table` with columns `branch` and `n_cols`.
    list_schemas = function() {
      data.table::rbindlist(lapply(names(private$schemas), function(nm) {
        data.table::data.table(branch = nm, n_cols = length(private$schemas[[nm]]))
      }))
    },

    #' @description
    #' Return the raw schema list for inspection.
    #' @return A named list of schemas.
    get_schemas = function() private$schemas,

    #' @description
    #' Create a new cohort branched from an existing cohort. The new
    #' cohort starts identical to its parent at the moment of branching;
    #' subsequent exclusions in the parent do not propagate to the child.
    #' @param name Character. Name of the new cohort.
    #' @param from Character. Name of the parent cohort.
    #' @param label Optional character. Display label for the cohort
    #'   (used in CONSORT diagrams and `list_cohorts()`); defaults to
    #'   `name`. May be refreshed silently across cache replays.
    #' @return The pipeline (invisibly).
    new_cohort = function(name, from, label = NULL) {
      if (!from %in% names(private$nodes)) {
        stop("new_cohort: unknown parent '", from, "'.", call. = FALSE)
      }

      if (name %in% names(private$nodes)) {
        existing <- private$nodes[[name]]
        existing_parent <- existing$parent
        if (identical(existing_parent, from)) {
          # Cache replay: cohort already exists with a matching parent.
          # Guard against a reordered script -- the parent must be at the
          # same replay position it occupied when this child originally
          # branched. If it isn't, the branch point moved and the cached
          # snapshot is stale; a correct rebuild is impossible because the
          # parent's intermediate status was never stored, so force a cache
          # invalidation rather than silently keeping wrong data.
          parent_cursor <- private$nodes[[from]]$replay_cursor %||%
            length(private$nodes[[from]]$log_entries)
          if (parent_cursor != existing$branched_at_log_len) {
            stop("new_cohort: cohort '", name, "' branched from '", from,
              "' at parent step ", existing$branched_at_log_len,
              ", but the current run is at parent step ", parent_cursor,
              " -- the operation order changed. Call $invalidate('", name,
              "') (or delete the cache) and rerun.", call. = FALSE)
          }
          # Label is presentation only; refresh it without invalidating.
          if (!is.null(label)) {
            private$nodes[[name]]$label <- label
          }
          private$nodes[[from]]$frozen <- TRUE
          return(invisible(self))
        }
        stop("new_cohort: cohort '", name, "' already exists",
          if (!is.na(existing_parent)) paste0(" (parent '", existing_parent, "')") else "",
          ". Call $invalidate('", name, "') to drop it first.",
          call. = FALSE)
      }

      parent_node <- private$nodes[[from]]
      private$nodes[[name]] <- list(
        parent              = from,
        label               = label %||% name,
        status              = parent_node$status,
        branched_at_status  = parent_node$status,
        log_entries         = parent_node$log_entries,
        branched_at_log_len = length(parent_node$log_entries),
        branched_at_n       = sum(parent_node$status == 0L),
        artifacts           = list(),
        frozen              = FALSE,
        replay_cursor       = length(parent_node$log_entries)
      )
      # Freeze the parent: branching from a cohort means its definition
      # must not change again, otherwise sibling branches would have
      # silently different parent states.
      private$nodes[[from]]$frozen <- TRUE
      if (private$auto_validate) self$validate()
      invisible(self)
    },

    #' @description
    #' Apply an exclusion predicate to a cohort and record the result on
    #' the exclusion log. The predicate is evaluated against the included
    #' subset of the base table; rows for which the predicate evaluates
    #' to `TRUE` are excluded with the supplied reason. `NA` predicate
    #' results are treated as `FALSE`.
    #' @param branch Character. Cohort to apply the exclusion to.
    #' @param reason Character. Human-readable reason recorded on the log.
    #' @param expr_str Character. R expression as a string (parsed with
    #'   `parse(text = ...)`). For example `"is.na(age) | age < 18"`.
    #' @return The pipeline (invisibly).
    exclude_and_track = function(branch, reason, expr_str) {
      if (!branch %in% names(private$nodes)) {
        stop("exclude_and_track: unknown branch '", branch, "'.", call. = FALSE)
      }
      if (!is.character(expr_str) || length(expr_str) != 1L) {
        stop("exclude_and_track: 'expr_str' must be a single character string.",
          call. = FALSE)
      }
      node <- private$nodes[[branch]]
      cursor <- node$replay_cursor %||% length(node$log_entries)

      # Cache replay: if the cursor still points inside the cohort's log,
      # the next entry must match exactly or we diverge.
      if (cursor < length(node$log_entries)) {
        cached <- node$log_entries[[cursor + 1L]]
        if (identical(cached$reason, reason) &&
            identical(cached$expr_str, expr_str)) {
          private$nodes[[branch]]$replay_cursor <- cursor + 1L
          return(invisible(self))
        }
        private$.invalidate_from(branch, cursor)
        node <- private$nodes[[branch]]
      }

      # Past the cached horizon. The freeze rule still protects against
      # adding a new exclusion to a cohort that has children/artifacts in
      # this session. Cache replay never reaches here for an unchanged
      # script, so the strict check stays intact.
      if (isTRUE(node$frozen)) {
        stop("exclude_and_track: cohort '", branch,
          "' is frozen (already has children or artifacts). ",
          "Apply all exclusions before branching from it or attaching ",
          "artifacts.", call. = FALSE)
      }
      step_num <- length(node$log_entries) + 1L
      included_idx <- which(node$status == 0L)

      if (length(included_idx) == 0L) {
        node$log_entries[[step_num]] <- list(
          step        = step_num,
          reason      = reason,
          expr_str    = expr_str,
          n_excluded  = 0L,
          n_remaining = 0L
        )
        node$replay_cursor <- step_num
        private$nodes[[branch]] <- node
        return(invisible(self))
      }

      expr_parsed <- parse(text = expr_str)[[1L]]
      mask <- tryCatch(
        private$base_dt[included_idx, eval(expr_parsed)],
        error = function(e) {
          stop(sprintf("exclude_and_track '%s': %s", reason, e$message),
            call. = FALSE)
        }
      )
      if (!is.logical(mask) || !is.null(dim(mask))) {
        stop(sprintf(
          "exclude_and_track '%s': predicate must return a logical vector, not %s.",
          reason, class(mask)[1L]), call. = FALSE)
      }
      if (length(mask) != length(included_idx)) {
        stop(sprintf(
          "exclude_and_track '%s': predicate returned length %d, expected %d.",
          reason, length(mask), length(included_idx)), call. = FALSE)
      }
      mask[is.na(mask)] <- FALSE
      exclude_idx <- included_idx[mask]
      if (length(exclude_idx) > 0L) {
        node$status[exclude_idx] <- step_num
      }
      n_remaining <- sum(node$status == 0L)
      node$log_entries[[step_num]] <- list(
        step        = step_num,
        reason      = reason,
        expr_str    = expr_str,
        n_excluded  = length(exclude_idx),
        n_remaining = n_remaining
      )
      node$replay_cursor <- step_num
      private$nodes[[branch]] <- node
      invisible(self)
    },

    #' @description
    #' Compute and cache a derived artifact on a cohort.
    #'
    #' `fn` may have either the legacy 2-argument signature
    #' `function(dt, sib)` or the 3-argument signature
    #' `function(dt, sib, argset)`. The 3-argument form pairs with the
    #' `argset` parameter to make the cache contract explicit: the cache
    #' key is `(name, from, body(fn), argset)`, so the artifact is
    #' recomputed only when one of those changes. With the 2-argument
    #' form, `fn` is invoked normally but `argset` is not used in the
    #' cache key (suitable for one-off scripts; not recommended when
    #' relying on `cache_file`).
    #'
    #' Note that the cache key uses `body(fn)` literally; if `fn` calls
    #' a helper that you change, the cache cannot detect that. Either
    #' include the helper's output / a version tag in `argset`, or call
    #' `$invalidate()` to force recompute.
    #' @param name Character. Artifact name (must be unique on the cohort).
    #' @param from Character. Cohort to attach the artifact to.
    #' @param fn Function with signature `function(dt, sib)` or
    #'   `function(dt, sib, argset)`. The return value becomes the
    #'   artifact.
    #' @param argset Optional named list. Explicit data dependencies of
    #'   `fn` (e.g. `list(outcomes = cfg$outcomes)`); participates in the
    #'   cache key. Use the 3-argument `fn` signature to read these out.
    #' @return The pipeline (invisibly).
    set_artifact = function(name, from, fn, argset = NULL) {
      if (!is.function(fn)) {
        stop("set_artifact: 'fn' must be a function.", call. = FALSE)
      }
      if (!from %in% names(private$nodes)) {
        stop("set_artifact: unknown cohort '", from, "'.", call. = FALSE)
      }
      # Compat shim: 2-arg fn keeps the old (dt, sib) signature; 3-arg
      # (or wider) fn gets argset passed as the third positional
      # argument. Functions with 4+ formals are accepted -- the extra
      # arguments must have defaults, since we never pass them.
      n_formals <- length(formals(fn))
      uses_argset <- n_formals >= 3L

      node <- private$nodes[[from]]
      cached_meta <- node$artifact_meta[[name]]
      if (!is.null(cached_meta)) {
        body_match <- identical(cached_meta$body, body(fn))
        argset_match <- identical(cached_meta$argset, argset)
        if (body_match && argset_match) {
          node$frozen <- TRUE
          private$nodes[[from]] <- node
          return(invisible(self))
        }
        # Divergent artifact: drop it and any artifacts declared after it
        # (subsequent ones may chain via `sib`).
        keep <- character()
        for (nm in names(node$artifacts)) {
          if (identical(nm, name)) break
          keep <- c(keep, nm)
        }
        node$artifacts <- node$artifacts[keep]
        node$artifact_meta <- node$artifact_meta[keep]
      } else if (name %in% names(node$artifacts)) {
        # Old-format cache (no meta): treat as miss.
        node$artifacts[[name]] <- NULL
      }

      result <- if (uses_argset) {
        fn(self$get_included(from), node$artifacts, argset)
      } else {
        fn(self$get_included(from), node$artifacts)
      }
      node$artifacts[[name]] <- result
      if (is.null(node$artifact_meta)) node$artifact_meta <- list()
      node$artifact_meta[[name]] <- list(
        body   = body(fn),
        argset = argset
      )
      # Freeze on first artifact: subsequent exclude_and_track would
      # silently invalidate the cached value.
      node$frozen <- TRUE
      private$nodes[[from]] <- node
      if (private$auto_validate) self$validate()
      invisible(self)
    },

    #' @description
    #' Return an independent copy of the included rows of a cohort. The
    #' returned `data.table` may be mutated freely without affecting the
    #' shared base table or any other cohort.
    #' @param cohort Character. Cohort name.
    #' @return A `data.table`.
    get_included = function(cohort) {
      if (!cohort %in% names(private$nodes)) {
        stop("get_included: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      node <- private$nodes[[cohort]]
      idx <- which(node$status == 0L)
      data.table::copy(private$base_dt[idx])
    },

    #' @description
    #' Return a copy of the full base table with a `.cohort_status` column
    #' reconstructed from this branch's exclusion history. Included rows
    #' are labeled `"included"`; excluded rows carry the reason of the
    #' first exclusion that caught them.
    #' @param cohort Character. Cohort name.
    #' @return A `data.table` of the same height as the base table, with
    #'   one extra column `.cohort_status`.
    get_everyone = function(cohort) {
      if (!cohort %in% names(private$nodes)) {
        stop("get_everyone: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      node <- private$nodes[[cohort]]
      reasons <- if (length(node$log_entries) == 0L) {
        character()
      } else {
        vapply(node$log_entries, function(e) e$reason, character(1L))
      }
      status_chr <- ifelse(node$status == 0L,
        "included",
        reasons[node$status])
      out <- data.table::copy(private$base_dt)
      out[, .cohort_status := status_chr]
      out
    },

    #' @description
    #' Retrieve a cached artifact from a cohort.
    #' @param cohort Character. Cohort name.
    #' @param name Character. Artifact name.
    #' @return The cached artifact (any type).
    get_artifact = function(cohort, name) {
      if (!cohort %in% names(private$nodes)) {
        stop("get_artifact: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      node <- private$nodes[[cohort]]
      if (!name %in% names(node$artifacts)) {
        stop("get_artifact: unknown artifact '", name, "' on cohort '",
          cohort, "'. Available: ",
          paste(names(node$artifacts), collapse = ", "),
          call. = FALSE)
      }
      node$artifacts[[name]]
    },

    #' @description
    #' Number of included rows in a cohort.
    #' @param cohort Character. Cohort name.
    #' @return Integer.
    n_included = function(cohort) {
      if (!cohort %in% names(private$nodes)) {
        stop("n_included: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      sum(private$nodes[[cohort]]$status == 0L)
    },

    #' @description
    #' Total number of rows in the shared base table.
    #' @return Integer.
    n_total = function() {
      if (is.null(private$base_dt)) 0L else nrow(private$base_dt)
    },

    #' @description
    #' Tabulate every cohort with its parent, sizes and number of own
    #' exclusion steps and artifacts.
    #' @return A `data.table` with one row per cohort.
    list_cohorts = function() {
      n_total <- self$n_total()
      data.table::rbindlist(lapply(names(private$nodes), function(nm) {
        node <- private$nodes[[nm]]
        n_inc <- sum(node$status == 0L)
        own_n_steps <- length(node$log_entries) - node$branched_at_log_len
        data.table::data.table(
          name           = nm,
          parent         = node$parent,
          n_total        = n_total,
          n_included     = n_inc,
          n_excluded     = n_total - n_inc,
          n_own_steps    = own_n_steps,
          n_artifacts    = length(node$artifacts),
          frozen         = isTRUE(node$frozen)
        )
      }))
    },

    #' @description
    #' Names of cached artifacts attached to a cohort.
    #' @param cohort Character. Cohort name.
    #' @return Character vector.
    list_artifacts = function(cohort) {
      if (!cohort %in% names(private$nodes)) {
        stop("list_artifacts: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      names(private$nodes[[cohort]]$artifacts)
    },

    #' @description
    #' Long-form table of exclusion log entries across all cohorts.
    #' Each cohort contributes only its own exclusion steps (steps
    #' inherited from the parent at branch time are reported under the
    #' parent, not duplicated).
    #' @return A `data.table` with columns `branch`, `parent`, `step`,
    #'   `reason`, `expr_str`, `n_excluded`, `n_remaining`.
    consort = function() {
      logs <- list()
      for (nm in names(private$nodes)) {
        node <- private$nodes[[nm]]
        own_idx <- seq_len(length(node$log_entries))[
          seq_along(node$log_entries) > node$branched_at_log_len
        ]
        if (length(own_idx) == 0L) next
        own_entries <- lapply(node$log_entries[own_idx], function(e) {
          data.table::data.table(
            step        = e$step,
            reason      = e$reason,
            expr_str    = e$expr_str %||% NA_character_,
            n_excluded  = e$n_excluded,
            n_remaining = e$n_remaining
          )
        })
        log <- data.table::rbindlist(own_entries)
        log[, branch := nm]
        log[, parent := node$parent]
        logs[[nm]] <- log
      }
      if (length(logs) == 0L) {
        return(data.table::data.table(
          branch      = character(),
          parent      = character(),
          step        = integer(),
          reason      = character(),
          expr_str    = character(),
          n_excluded  = integer(),
          n_remaining = integer()
        ))
      }
      out <- data.table::rbindlist(logs)
      data.table::setcolorder(out, c("branch", "parent", "step",
        "reason", "expr_str", "n_excluded", "n_remaining"))
      out
    },

    #' @description
    #' Render one or more CONSORT panels for cohort flows. Each panel
    #' walks a sequence of cohort names, lumping the named cohorts'
    #' exclusion steps into bullet blocks.
    #'
    #' Most users want `$plot()` instead, which auto-discovers every
    #' root-to-leaf path in the tree and lays them out automatically.
    #' `$draw_consort_panels()` is the manual escape hatch for custom
    #' layouts and labels.
    #' @param panels A named list. Each element is either a character
    #'   vector of cohort names (interpreted as the panel's main flow)
    #'   or a list with components `flow` (character) and optional
    #'   `side_branches` (named character of identity-only branches that
    #'   merge into the spine).
    #' @param file Optional character path. If supplied, the rendered
    #'   plot is written to a `.pdf` or `.png` file. Otherwise the
    #'   plot is drawn on the active device.
    #' @param ncol Optional integer. Number of panels per row.
    #' @param width,height Optional numeric (inches). File dimensions.
    #' @param text_width Integer. Wrap width for box text.
    #' @param title_fontsize Numeric. Title fontsize for each panel.
    #' @return A list of grobs (invisibly).
    draw_consort_panels = function(panels, file = NULL, ncol = NULL,
                                   width = NULL, height = NULL,
                                   text_width = 40, title_fontsize = 14) {
      .draw_consort_panels_impl(
        panels         = panels,
        nodes          = private$nodes,
        file           = file,
        ncol           = ncol,
        width          = width,
        height         = height,
        text_width     = text_width,
        title_fontsize = title_fontsize
      )
    },

    #' @description
    #' Plot a CONSORT diagram of the cohort tree.
    #'
    #' With no arguments, plots one panel per cohort. Each panel walks
    #' the root-to-cohort path automatically and uses cohort names as
    #' box labels. With one or more cohort names, plots only those.
    #'
    #' This is the default convenience entry point. Use
    #' `$draw_consort_panels()` for custom labels or layouts.
    #' @param cohorts Optional character vector of cohort names. If
    #'   omitted, every cohort is plotted.
    #' @param file Optional `.pdf`/`.png` path. If supplied, the plot is
    #'   written to that file. Otherwise it is drawn on the active device.
    #' @param ncol,width,height,text_width,title_fontsize Optional layout
    #'   overrides; see `$draw_consort_panels()`.
    #' @return A list of grobs (invisibly).
    plot = function(cohorts = NULL, file = NULL, ncol = NULL,
                    width = NULL, height = NULL,
                    text_width = 40, title_fontsize = 14) {
      if (length(private$nodes) == 0L) {
        stop("plot: pipeline has no cohorts. Construct with ",
          "CohortPipeline$new(dt) first.", call. = FALSE)
      }
      if (is.null(cohorts)) {
        cohorts <- names(private$nodes)
      } else {
        unknown <- setdiff(cohorts, names(private$nodes))
        if (length(unknown) > 0L) {
          stop("plot: unknown cohort(s): ",
            paste(unknown, collapse = ", "), call. = FALSE)
        }
      }
      panels <- private$build_default_panels(cohorts)
      self$draw_consort_panels(
        panels         = panels,
        file           = file,
        ncol           = ncol,
        width          = width,
        height         = height,
        text_width     = text_width,
        title_fontsize = title_fontsize
      )
    },

    #' @description
    #' Concise text summary of the cohort tree, exclusion counts, and
    #' attached artifacts.
    #' @param ... Unused.
    #' @return The pipeline (invisibly).
    print = function(...) {
      cat("<CohortPipeline>\n")
      if (length(private$nodes) == 0L) {
        cat("  (empty -- construct with CohortPipeline$new(dt) to install a base table)\n")
        return(invisible(self))
      }
      n_total <- self$n_total()
      for (nm in names(private$nodes)) {
        node <- private$nodes[[nm]]
        indent <- if (is.na(node$parent)) "" else "  "
        n_inc <- sum(node$status == 0L)
        own_n_steps <- length(node$log_entries) - node$branched_at_log_len
        if (is.na(node$parent)) {
          cat(sprintf(
            "%s%s: loaded = %s, included = %s, excluded = %s, %d exclusion step(s)\n",
            indent, nm,
            format(n_total, big.mark = ","),
            format(n_inc, big.mark = ","),
            format(n_total - n_inc, big.mark = ","),
            own_n_steps
          ))
        } else {
          n_own_excluded <- node$branched_at_n - n_inc
          cat(sprintf(
            "%s%s: branched from %s at n = %s, own excluded = %s, included = %s, %d own step(s)\n",
            indent, nm, node$parent,
            format(node$branched_at_n, big.mark = ","),
            format(n_own_excluded, big.mark = ","),
            format(n_inc, big.mark = ","),
            own_n_steps
          ))
        }
        for (art in names(node$artifacts)) {
          cat(sprintf("%s  $ %s\n", indent, art))
        }
      }
      invisible(self)
    },

    #' @description
    #' Persist the pipeline to its `cache_file` (set at construction).
    #' On the next `CohortPipeline$new(dt, cache_file = ...)` with the same
    #' file, the saved state is restored and re-issued operations replay
    #' from the cache; only divergent operations recompute. Idempotent
    #' beyond the file write.
    #' @param file Optional override for the cache file path.
    #' @return The pipeline (invisibly).
    save = function(file = NULL) {
      path <- file %||% private$cache_file
      if (is.null(path)) {
        stop("save: no cache_file set. Pass one to CohortPipeline$new() or ",
          "to $save(file = ...) explicitly.", call. = FALSE)
      }
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(list(
        cache_version = .COHORT_CACHE_VERSION,
        base_dt       = private$base_dt,
        base_dt_hash  = private$base_dt_hash,
        nodes         = private$nodes,
        schemas       = private$schemas
      ), file = path)
      invisible(self)
    },

    #' @description
    #' Manually invalidate a cached cohort (drops the cohort and every
    #' descendant) or a single artifact. Use when a helper function called
    #' from inside a `set_artifact` `fn` has changed -- the cache key
    #' (`body(fn)` + argset) cannot detect that automatically.
    #' @param cohort Character. Cohort to invalidate.
    #' @param artifact Optional character. If supplied, only the named
    #'   artifact (and any artifacts declared after it on the same cohort)
    #'   is dropped.
    #' @return The pipeline (invisibly).
    invalidate = function(cohort, artifact = NULL) {
      if (!cohort %in% names(private$nodes)) {
        stop("invalidate: unknown cohort '", cohort, "'.", call. = FALSE)
      }
      if (is.null(artifact)) {
        private$.drop_subtree(cohort)
      } else {
        node <- private$nodes[[cohort]]
        if (!artifact %in% names(node$artifacts)) {
          stop("invalidate: cohort '", cohort, "' has no artifact '",
            artifact, "'.", call. = FALSE)
        }
        keep <- character()
        for (nm in names(node$artifacts)) {
          if (identical(nm, artifact)) break
          keep <- c(keep, nm)
        }
        node$artifacts <- node$artifacts[keep]
        node$artifact_meta <- node$artifact_meta[keep]
        private$nodes[[cohort]] <- node
      }
      invisible(self)
    }
  ),
  private = list(
    base_dt        = NULL,
    nodes          = NULL,
    schemas        = NULL,
    auto_validate  = FALSE,
    cache_file     = NULL,
    base_dt_hash   = NULL,

    # Install dt as the shared base table and create the root cohort.
    # Called from the constructor; not part of the public API.
    install_base = function(dt, label) {
      stopifnot(is.data.table(dt))
      private$base_dt <- data.table::copy(dt)
      n <- nrow(private$base_dt)
      private$nodes <- list(
        root = list(
          parent              = NA_character_,
          label               = label,
          status              = integer(n),                      # all 0L (included)
          branched_at_status  = integer(n),
          log_entries         = list(),
          branched_at_log_len = 0L,
          branched_at_n       = n,
          artifacts           = list(),
          frozen              = FALSE,
          replay_cursor       = 0L
        )
      )
    },

    # Read-only access to the shared base table for plotting helpers.
    get_base_dt = function() private$base_dt,
    get_nodes   = function() private$nodes,

    # Names of leaf cohorts (cohorts with no children).
    leaf_cohorts = function() {
      parents <- vapply(private$nodes, function(n) {
        if (is.null(n$parent) || is.na(n$parent)) NA_character_ else n$parent
      }, character(1L))
      has_child <- names(private$nodes) %in% parents
      names(private$nodes)[!has_child]
    },

    # Build the panels list for $plot(). One panel per cohort, walking
    # the root-to-cohort path. Box labels and panel titles use each
    # cohort's display label (falling back to its identifier).
    build_default_panels = function(cohorts) {
      label_of <- function(nm) {
        private$nodes[[nm]]$label %||% nm
      }
      panels <- list()
      for (co in cohorts) {
        path <- private$ancestor_path(co)  # root-first character vector
        names(path) <- vapply(path, label_of, character(1L))
        panels[[label_of(co)]] <- path
      }
      panels
    },

    # Walk parents from a cohort up to the root; return a root-first
    # character vector.
    ancestor_path = function(cohort) {
      out <- character()
      cur <- cohort
      while (!is.null(cur) && !is.na(cur)) {
        out <- c(cur, out)
        parent <- private$nodes[[cur]]$parent
        cur <- if (is.null(parent) || is.na(parent)) NULL else parent
      }
      out
    },

    # All transitive descendants of a cohort.
    .descendants = function(name) {
      result <- character()
      to_check <- name
      while (length(to_check) > 0L) {
        next_check <- character()
        for (cur in to_check) {
          kids <- vapply(private$nodes, function(n) {
            !is.na(n$parent) && identical(n$parent, cur)
          }, logical(1L))
          kids <- names(private$nodes)[kids]
          result <- c(result, kids)
          next_check <- c(next_check, kids)
        }
        to_check <- next_check
      }
      result
    },

    # Drop a cohort and every descendant from the node store.
    .drop_subtree = function(name) {
      for (n in c(name, private$.descendants(name))) {
        private$nodes[[n]] <- NULL
      }
    },

    # Truncate `branch`'s log at absolute index `cursor`, replay the kept
    # own entries from the at-branch status, drop artifacts, and cascade
    # invalidate any descendants that branched after the cutoff. `cursor`
    # is an ABSOLUTE log index (it counts the inherited prefix), matching
    # `replay_cursor` and the absolute step numbers stored in `status`.
    .invalidate_from = function(branch, cursor) {
      node <- private$nodes[[branch]]
      keep_len <- cursor
      if (keep_len < length(node$log_entries)) {
        node$log_entries <- node$log_entries[seq_len(keep_len)]
      }
      # Re-derive status by replaying the kept own entries against the
      # at-branch status. Inherited entries (positions <= branched_at_log_len)
      # are part of the parent's history and don't need re-execution
      # here -- the parent applied them already.
      status <- node$branched_at_status
      n_own_keep <- cursor - node$branched_at_log_len
      if (n_own_keep > 0L) {
        for (i in seq_len(n_own_keep)) {
          step_idx <- node$branched_at_log_len + i
          entry    <- node$log_entries[[step_idx]]
          included_idx <- which(status == 0L)
          if (length(included_idx) > 0L) {
            expr <- parse(text = entry$expr_str)[[1L]]
            mask <- private$base_dt[included_idx, eval(expr)]
            mask[is.na(mask)] <- FALSE
            ex <- included_idx[as.logical(mask)]
            if (length(ex) > 0L) status[ex] <- step_idx
          }
        }
      }
      node$status        <- status
      node$artifacts     <- list()
      node$artifact_meta <- list()
      node$frozen        <- FALSE
      node$replay_cursor <- cursor
      private$nodes[[branch]] <- node

      # Cascade: descendants that branched after the cutoff inherited the
      # now-stale entries. Drop them; the user's script will recreate
      # them on subsequent calls (which will fall through to fresh
      # construction since the names will no longer exist).
      for (d in private$.descendants(branch)) {
        if (private$nodes[[d]]$branched_at_log_len > keep_len) {
          private$nodes[[d]] <- NULL
        }
      }
    }
  )
)

# Cache schema version. Bump on any incompatible change to the
# serialised structure (new fields, renamed fields, etc.).
.COHORT_CACHE_VERSION <- 3L

# Local %||%; not exported. (R 4.4 introduced this in base; we keep our
# own copy for portability with the declared R >= 3.5.0.)
`%||%` <- function(a, b) if (is.null(a)) b else a
