linters <- all_linters(

    # grates specific ---------------------------------------------------------
    function_argument_linter = NULL, # risky to change now

    # general -----------------------------------------------------------------

    # seems ok to me
    brace_linter(allow_single_line = TRUE),

    # often like to have alternative code flagged to know when we have considered
    # tradeoffs for a particular approach
    commented_code_linter = NULL,

    # Showing the call is useful and prefer to rethrow / handle errors if
    # wanting a better user experience
    condition_call_linter(display_call = TRUE),

    # Debatable
    cyclocomp_linter = NULL,

    # although I'm receptive to using L for integers, the same does not hold for
    # decimal where it feels like unwanted noise.
    implicit_integer_linter = NULL,

    # NOTE: Currently we can only assign within an `if` statement when both
    # allow_scoped = TRUE *AND* the object is ONLY used in the following branch
    # (unless reassigned). lintr does not treat `if` as a function so we cannot
    # put it in the `except` argument ... :(
    implicit_assignment_linter(
        except = c("expect_error", "expect_snapshot_error", "within"),
        allow_scoped = TRUE,
        allow_paren_print = TRUE
    ),

    # Seems to give false positives. Will revisit.
    indentation_linter = NULL,

    # Can be stylistically annoying when names are mixed (i.e. some need quoting
    # and some don't).
    keyword_quote_linter = NULL,

    # Whilst I do prefer to keep library calls together at the top there are
    # exceptions so this adds unwanted noise for something easily seen during
    # review anyway.
    library_call_linter = NULL,

    # This is a reasonable compromise in terms of flagging. Less or more can
    # both be appropriate
    line_length_linter(length = 100L),

    # Too many false positives
    nonportable_path_linter = NULL,

    # dubious especially considering methods
    object_length_linter = NULL,

    # Too noisy - flexibility is good especially as I slowly come round to
    # camelCase and it's variations
    object_name_linter = NULL,

    # Annoying when it comes to NULL assignments for data.table NSE/CRAN workaround
    object_usage_linter = NULL,

    # Too restrictive
    object_overwrite_linter = NULL,

    # It's good to have them flagged but perhaps better via a specific hook?
    todo_comment_linter = NULL,

    # User dfe
    undesirable_function_linter = NULL,
    undesirable_function_name_linter = undesirable_function_linter(modify_defaults(
        defaults = default_undesirable_functions,
        library = NULL,
        structure = NULL # unless we find it to be a bottleneck in our own code
    ))

)

exclusions <- list(
    "tests/testthat/*.R" = list(line_length_linter = Inf),
    "tests/testthat.R" = list(unused_import_linter = Inf),
    "vignettes/*.Rmd" = list(implicit_assignment_linter = Inf)
)
