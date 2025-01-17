linters <- list(
    # Description:
    #
    #      Check that no absolute paths are used (e.g. "/var", "C:\System",
    #      "~/docs").
    #
    # Arguments:
    #
    #      lax: Less stringent linting, leading to fewer false positives. If
    #           'TRUE', only lint path strings, which
    #
    #             • contain at least two path elements, with one having at
    #               least two characters and
    #
    #             • contain only alphanumeric chars (including UTF-8),
    #               spaces, and win32-allowed punctuation
    #
    absolute_path_linter(lax = TRUE),

    # Description:
    #
    #      'anyDuplicated()' exists as a replacement for
    #      'any(duplicated(.))', which is more efficient for simple objects,
    #      and is at worst equally efficient. Therefore, it should be used in
    #      all situations instead of the latter.
    #
    any_duplicated_linter(),

    # Description:
    #
    #      'anyNA()' exists as a replacement for 'any(is.na(x))' which is
    #      more efficient for simple objects, and is at worst equally
    #      efficient. Therefore, it should be used in all situations instead
    #      of the latter.
    #
    any_is_na_linter(),

    # Description:
    #
    #      Check that <- is always used for assignment.
    #
    # Arguments:
    #
    # allow_cascading_assign: Logical, default 'TRUE'. If 'FALSE', '<<-' and
    #           ->> are not allowed.
    #
    # allow_right_assign: Logical, default 'FALSE'. If 'TRUE', -> and ->> are
    #           allowed.
    #
    # allow_trailing: Logical, default 'TRUE'. If 'FALSE' then assignments
    #           aren't allowed at end of lines.
    #
    # allow_pipe_assign: Logical, default 'FALSE'. If 'TRUE', magrittr's %<>%
    #           assignment is allowed.
    #
    assignment_linter(allow_cascading_assign = TRUE, allow_right_assign = FALSE, allow_trailing = TRUE, allow_pipe_assign = FALSE),

    # Description:
    #
    #      Check for usage of unavailable functions. Not reliable for testing
    #      r-devel dependencies.
    #
    # Arguments:
    #
    # r_version: Minimum R version to test for compatibility
    #
    #   except: Character vector of functions to be excluded from linting.
    #           Use this to list explicitly defined backports, e.g. those
    #           imported from the '{backports}' package or manually defined
    #           in your package.
    #
    backport_linter(r_version = getRversion(), except = character()),

    # Description:
    #
    #      'length(which(x == y)) == 0' is the same as '!any(x == y)', but
    #      the latter is more readable and more efficient.
    #
    boolean_arithmetic_linter(),

    # Description:
    #
    #      Perform various style checks related to placement and spacing of
    #      curly braces:
    #
    # Arguments:
    #
    # allow_single_line: if 'TRUE', allow an open and closed curly pair on
    #           the same line.
    #
    brace_linter(allow_single_line = TRUE),

    # Description:
    #
    #      Usage like 'class(x) == "character"' is prone to error since class
    #      in R is in general a vector. The correct version for S3 classes is
    #      'inherits()': 'inherits(x, "character")'. Often, class 'k' will
    #      have an 'is.' equivalent, for example 'is.character()' or
    #      'is.data.frame()'.
    #
    class_equals_linter(),

    # Description:
    #
    #      Check that all commas are followed by spaces, but do not have
    #      spaces before them.
    #
    # Arguments:
    #
    # allow_trailing: If 'TRUE', the linter allows a comma to be followed
    #           directly by a closing bracket without a space.
    #
    commas_linter(allow_trailing = FALSE),

    # Description:
    #
    #      Check that there is no commented code outside roxygen blocks.
    #
    commented_code_linter(),

    # Description:
    #
    #      This linter discourages combining condition functions like
    #      'stop()' with string concatenation functions 'paste()' and
    #      'paste0()'. This is because
    #
    #         • 'stop(paste0(...))' is redundant as it is exactly equivalent
    #           to 'stop(...)'
    #
    #         • 'stop(paste(...))' is similarly equivalent to 'stop(...)'
    #           with separators (see examples)
    #
    #      The same applies to the other default condition functions as well,
    #      i.e., 'warning()', 'message()', and 'packageStartupMessage()'.
    #
    condition_message_linter(),

    # Description:
    #
    #      For readability of test outputs, testing only one thing per call
    #      to 'testthat::expect_true()' is preferable, i.e., expect_true(A);
    #      expect_true(B) is better than 'expect_true(A && B)', and
    #      expect_false(A); expect_false(B) is better than 'expect_false(A ||
    #      B)'.
    #
    # Arguments:
    #
    # allow_named_stopifnot: Logical, 'TRUE' by default. If 'FALSE', "named"
    #           calls to 'stopifnot()', available since R 4.0.0 to provide
    #           helpful messages for test failures, are also linted.
    #
    # allow_filter: Character naming the method for linting calls to
    #           'filter()'. The default, '"never"', means 'filter()' and
    #           'dplyr::filter()' calls are linted; '"not_dplyr"' means only
    #           'dplyr::filter()' calls are linted; and '"always"' means no
    #           calls to 'filter()' are linted. Calls like 'stats::filter()'
    #           are never linted.
    #
    conjunct_test_linter(allow_named_stopifnot = TRUE, allow_filter = c("never", "not_dplyr", "always")),

    # Description:
    #
    #      'stopifnot()' accepts any number of tests, so sequences like
    #      stopifnot(x); stopifnot(y) are redundant. Ditto for tests using
    #      'assertthat::assert_that()' without specifying msg=.
    #
    consecutive_assertion_linter(),

    # Description:
    #
    #      Check for overly complicated expressions. See
    #      'cyclocomp::cyclocomp()'.
    #
    # Arguments:
    #
    # complexity_limit: Maximum cyclomatic complexity, default 15.
    #           Expressions more complex than this are linted. See
    #           'cyclocomp::cyclocomp()'.
    #
    cyclocomp_linter(complexity_limit = 15L),

    # Description:
    #
    #      Check for duplicate arguments in function calls. Some cases are
    #      run-time errors (e.g. 'mean(x = 1:5, x = 2:3)'), otherwise this
    #      linter is used to discourage explicitly providing duplicate names
    #      to objects (e.g. 'c(a = 1, a = 2)'). Duplicate-named objects are
    #      hard to work with programmatically and should typically be
    #      avoided.
    #
    # Arguments:
    #
    #   except: A character vector of function names as exceptions. Defaults
    #           to functions that allow sequential updates to variables,
    #           currently 'dplyr::mutate()' and 'dplyr::transmute()'.
    #
    duplicate_argument_linter(except = c("mutate", "transmute")),

    # Description:
    #
    #      Assignment of '{}' is the same as assignment of 'NULL'; use the
    #      latter for clarity. Closely related:
    #      'unnecessary_concatenation_linter()'.
    #
    empty_assignment_linter(),

    # Description:
    #
    #      Check for 'x == NA', 'x != NA' and 'x %in% NA'. Such usage is
    #      almost surely incorrect - checks for missing values should be done
    #      with 'is.na()'.
    #
    equals_na_linter(),

    # Description:
    #
    #      'testthat::expect_gt()', 'testthat::expect_gte()',
    #      'testthat::expect_lt()', 'testthat::expect_lte()', and
    #      'testthat::expect_equal()' exist specifically for testing
    #      comparisons between two objects. 'testthat::expect_true()' can
    #      also be used for such tests, but it is better to use the tailored
    #      function instead.
    #
    expect_comparison_linter(),

    # Description:
    #
    #      This linter enforces the usage of 'testthat::expect_identical()'
    #      as the default expectation for comparisons in a testthat suite.
    #      'expect_true(identical(x, y))' is an equivalent but unadvised
    #      method of the same test. Further, 'testthat::expect_equal()'
    #      should only be used when 'expect_identical()' is inappropriate,
    #      i.e., when 'x' and 'y' need only be _numerically equivalent_
    #      instead of fully identical (in which case, provide the tolerance=
    #      argument to 'expect_equal()' explicitly). This also applies when
    #      it's inconvenient to check full equality (e.g., names can be
    #      ignored, in which case 'ignore_attr = "names"' should be supplied
    #      to 'expect_equal()' (or, for 2nd edition, 'check.attributes =
    #      FALSE').
    #
    expect_identical_linter(),

    # Description:
    #
    #      'testthat::expect_length()' exists specifically for testing the
    #      'length()' of an object. 'testthat::expect_equal()' can also be
    #      used for such tests, but it is better to use the tailored function
    #      instead.
    #
    expect_length_linter(),

    # Description:
    #
    #      'testthat::expect_named()' exists specifically for testing the
    #      'names()' of an object. 'testthat::expect_equal()' can also be
    #      used for such tests, but it is better to use the tailored function
    #      instead.
    #
    expect_named_linter(),

    # Description:
    #
    #      'testthat::expect_false()' exists specifically for testing that an
    #      output is 'FALSE'. 'testthat::expect_true()' can also be used for
    #      such tests by negating the output, but it is better to use the
    #      tailored function instead. The reverse is also true - use
    #      'expect_false(A)' instead of 'expect_true(!A)'.
    #
    expect_not_linter(),

    # Description:
    #
    #      Require usage of 'expect_null(x)' over 'expect_equal(x, NULL)' and
    #      similar usages.
    #
    expect_null_linter(),

    # Description:
    #
    #      'testthat::expect_s3_class()' exists specifically for testing the
    #      class of S3 objects. 'testthat::expect_equal()',
    #      'testthat::expect_identical()', and 'testthat::expect_true()' can
    #      also be used for such tests, but it is better to use the tailored
    #      function instead.
    #
    expect_s3_class_linter(),

    # Description:
    #
    #      'testthat::expect_s4_class()' exists specifically for testing the
    #      class of S4 objects. 'testthat::expect_true()' can also be used
    #      for such tests, but it is better to use the tailored function
    #      instead.
    #
    expect_s4_class_linter(),

    # Description:
    #
    #      'testthat::expect_true()' and 'testthat::expect_false()' exist
    #      specifically for testing the 'TRUE'/'FALSE' value of an object.
    #      'testthat::expect_equal()' and 'testthat::expect_identical()' can
    #      also be used for such tests, but it is better to use the tailored
    #      function instead.
    #
    expect_true_false_linter(),

    # Description:
    #
    #      'testthat::expect_type()' exists specifically for testing the
    #      storage type of objects. 'testthat::expect_equal()',
    #      'testthat::expect_identical()', and 'testthat::expect_true()' can
    #      also be used for such tests, but it is better to use the tailored
    #      function instead.
    #
    expect_type_linter(),

    # Description:
    #
    #      Check that the '[[' operator is used when extracting a single
    #      element from an object, not '[' (subsetting) nor '$' (interactive
    #      use).
    #
    extraction_operator_linter(),

    # Description:
    #
    #      Invoking a regular expression engine is overkill for cases when
    #      the search pattern only involves static patterns.
    #
    # Arguments:
    #
    # allow_unescaped: Logical, default 'FALSE'. If 'TRUE', only patterns
    #           that require regex escapes (e.g. '"\\$"' or '"[$]"') will be
    #           linted. See examples.
    #
    fixed_regex_linter(allow_unescaped = FALSE),

    # Description:
    #
    #      for (x in x) is a poor choice of indexing variable. This
    #      overwrites 'x' in the calling scope and is confusing to read.
    #
    for_loop_index_linter(),

    # Description:
    #
    #      Check that arguments with defaults come last in all function
    #      declarations, as per the tidyverse design guide.
    #
    #      Changing the argument order can be a breaking change. An
    #      alternative to changing the argument order is to instead set the
    #      default for such arguments to 'NULL'.
    #
    function_argument_linter(),

    # Description:
    #
    #      Check that all left parentheses in a function call do not have
    #      spaces before them (e.g. 'mean (1:3)'). Although this is
    #      syntactically valid, it makes the code difficult to read.
    #
    function_left_parentheses_linter(),

    # Description:
    #
    #      'return(x <- ...)' is either distracting (because 'x' is ignored),
    #      or confusing (because assigning to 'x' has some side effect that
    #      is muddled by the dual-purpose expression).
    #
    function_return_linter(),

    # Description:
    #
    #      'if (!A) x else y' is the same as 'if (A) y else x', but the
    #      latter is easier to reason about in the else case. The former
    #      requires double negation that can be avoided by switching the
    #      statement order.
    #
    # Arguments:
    #
    # exceptions: Character vector of calls to exclude from linting. By
    #           default, 'is.null()', 'is.na()', and 'missing()' are excluded
    #           given the common idiom '!is.na(x)' as "x is present".
    #
    if_not_else_linter(exceptions = c("is.null", "is.na", "missing")),

    # Description:
    #
    #      'ifelse(x > M, M, x)' is the same as 'pmin(x, M)', but harder to
    #      read and requires several passes over the vector.
    #
    ifelse_censor_linter(),

    # Description:
    #
    #      Assigning inside function calls makes the code difficult to read,
    #      and should be avoided, except for functions that capture
    #      side-effects (e.g. 'capture.output()').
    #
    # Arguments:
    #
    #   except: A character vector of functions to be excluded from linting.
    #
    # allow_lazy: logical, default 'FALSE'. If 'TRUE', assignments that only
    #           trigger conditionally (e.g. in the RHS of '&&' or '||'
    #           expressions) are skipped.
    #
    # allow_scoped: Logical, default 'FALSE'. If 'TRUE', "scoped
    #           assignments", where the object is assigned in the statement
    #           beginning a branch and used only within that branch, are
    #           skipped.
    #
    implicit_assignment_linter(except = c("bquote", "expression", "expr", "quo", "quos", "quote"), allow_lazy = FALSE, allow_scoped = FALSE),

    # Description:
    #
    #      Check that integers are explicitly typed using the form '1L'
    #      instead of '1'.
    #
    # Arguments:
    #
    # allow_colon: Logical, default 'FALSE'. If 'TRUE', expressions involving
    #           ':' won't throw a lint regardless of whether the inputs are
    #           implicitly integers.
    #
    implicit_integer_linter(allow_colon = FALSE),

    # Description:
    #
    #      Check that indentation is consistent
    #
    # Arguments:
    #
    #   indent: Number of spaces, that a code block should be indented by
    #           relative to its parent code block. Used for multi-line code
    #           blocks ('{ ... }'), function calls ('( ... )') and
    #           extractions ([ ... ], [[ ... ]]). Defaults to 2.
    #
    # hanging_indent_style: Indentation style for multi-line function calls
    #           with arguments in their first line. Defaults to tidyverse
    #           style, i.e. a block indent is used if the function call
    #           terminates with ) on a separate line and a hanging indent if
    #           not. Note that function multi-line function calls without
    #           arguments on their first line will always be expected to have
    #           block-indented arguments. If 'hanging_indent_style' is
    #           '"tidy"', multi-line function definitions are expected to be
    #           double-indented if the first line of the function definition
    #           contains no arguments and the closing parenthesis is not on
    #           its own line.
    #
    #           # complies to any style
    #           map(
    #             x,
    #             f,
    #             additional_arg = 42
    #           )
    #
    #           # complies to "tidy" and "never"
    #           map(x, f,
    #             additional_arg = 42
    #           )
    #
    #           # complies to "always"
    #           map(x, f,
    #               additional_arg = 42
    #           )
    #
    #           # complies to "tidy" and "always"
    #           map(x, f,
    #               additional_arg = 42)
    #
    #           # complies to "never"
    #           map(x, f,
    #             additional_arg = 42)
    #
    #           # complies to "tidy"
    #           function(
    #               a,
    #               b) {
    #             # body
    #           }
    #
    # assignment_as_infix: Treat <- as a regular (i.e. left-associative)
    #           infix operator? This means, that infix operators on the right
    #           hand side of an assignment do not trigger a second level of
    #           indentation:
    #
    #           # complies to any style
    #           variable <- a %+%
    #             b %+%
    #             c
    #
    #           # complies to assignment_as_infix = TRUE
    #           variable <-
    #             a %+%
    #             b %+%
    #             c
    #
    #           # complies to assignment_as_infix = FALSE
    #           variable <-
    #             a %+%
    #               b %+%
    #               c
    #
    indentation_linter(indent = 4L, hanging_indent_style = c("tidy", "always", "never"), assignment_as_infix = TRUE),

    # Description:
    #
    #      Check that infix operators are surrounded by spaces. Enforces the
    #      corresponding Tidyverse style guide rule; see
    #      <https://style.tidyverse.org/syntax.html#infix-operators>.
    #
    # Arguments:
    #
    # exclude_operators: Character vector of operators to exclude from
    #           consideration for linting. Default is to include the
    #           following "low-precedence" operators: '+', '-', '~', '>',
    #           '>=', '<', '<=', '==', '!=', '&', '&&', '|', '||', <-, :=,
    #           <<-, ->, ->>, '=', '/', '*', and any infix operator (exclude
    #           infixes by passing '"%%"'). Note that '"="' here includes
    #           three different operators, from the parser's point of view.
    #           To lint only some of these, pass the corresponding parse tags
    #           (i.e., some of '"EQ_ASSIGN"', '"EQ_SUB"', and '"EQ_FORMALS"';
    #           see 'utils::getParseData()').
    #
    # allow_multiple_spaces: Logical, default 'TRUE'. If 'FALSE', usage like
    #           'x = 2' will also be linted; excluded by default because such
    #           usage can sometimes be used for better code alignment, as is
    #           allowed by the style guide.
    #
    infix_spaces_linter(exclude_operators = NULL, allow_multiple_spaces = TRUE),

    # Description:
    #
    #      'as.Date(c(a, b))' is logically equivalent to 'c(as.Date(a),
    #      as.Date(b))'. The same equivalence holds for several other
    #      vectorized functions like 'as.POSIXct()' and math functions like
    #      'sin()'. The former is to be preferred so that the most expensive
    #      part of the operation ('as.Date()') is applied only once.
    #
    inner_combine_linter(),

    # Description:
    #
    #      'is.numeric()' returns 'TRUE' when 'typeof(x)' is 'double' or
    #      'integer' - testing 'is.numeric(x) || is.integer(x)' is thus
    #      redundant.
    #
    is_numeric_linter(),

    # Description:
    #
    #      Any valid symbol can be used as a keyword argument to an R
    #      function call. Sometimes, it is necessary to quote (or backtick)
    #      an argument that is not an otherwise valid symbol (e.g. creating a
    #      vector whose names have spaces); besides this edge case, quoting
    #      should not be done.
    #
    keyword_quote_linter(),

    # Description:
    #
    #      'length(levels(x))' is the same as 'nlevels(x)', but harder to
    #      read.
    #
    length_levels_linter(),

    # Description:
    #
    #      Usage like 'length(x == 0)' is a mistake. If you intended to check
    #      'x' is empty, use 'length(x) == 0'. Other mistakes are possible,
    #      but running 'length()' on the outcome of a logical comparison is
    #      never the best choice.
    #
    length_test_linter(),

    # Description:
    #
    #      'lengths()' is a function that was added to base R in version
    #      3.2.0 to get the length of each element of a list. It is
    #      equivalent to 'sapply(x, length)', but faster and more readable.
    #
    lengths_linter(),

    # Description:
    #
    #      Force library calls to all be at the top of the script.
    #
    # Arguments:
    #
    # allow_preamble: Logical, default 'TRUE'. If 'FALSE', no code is allowed
    #           to precede the first 'library()' call, otherwise some setup
    #           code is allowed, but all 'library()' calls must follow
    #           consecutively after the first one.
    #
    library_call_linter(allow_preamble = TRUE),

    # Description:
    #
    #      Check that the line length of both comments and code is less than
    #      'length'.
    #
    # Arguments:
    #
    #   length: maximum line length allowed. Default is 80L (Hollerith
    #           limit).
    #
    line_length_linter(length = 80L),

    # Description:
    #
    #      'as.integer(1)' (or 'rlang::int(1)') is the same as '1L' but the
    #      latter is more concise and gets typed correctly at compilation.
    #
    literal_coercion_linter(),

    # Description:
    #
    #      'colSums()' and 'rowSums()' are clearer and more performant
    #      alternatives to 'apply(x, 2, sum)' and 'apply(x, 1, sum)'
    #      respectively in the case of 2D arrays, or matrices
    #
    matrix_apply_linter(),

    # Description:
    #
    #      Check for missing arguments in function calls (e.g.
    #      'stats::median(1:10, )').
    #
    # Arguments:
    #
    #   except: a character vector of function names as exceptions.
    #
    # allow_trailing: always allow trailing empty arguments?
    #
    missing_argument_linter(except = c("alist", "quote", "switch"), allow_trailing = FALSE),

    # Description:
    #
    #      Check for missing packages in 'library()', 'require()',
    #      'loadNamespace()', and 'requireNamespace()' calls.
    #
    missing_package_linter(),

    # Description:
    #
    #      Check for missing packages and symbols in namespace calls. Note
    #      that using 'check_exports=TRUE' or 'check_nonexports=TRUE' will
    #      load packages used in user code so it could potentially change the
    #      global state.
    #
    # Arguments:
    #
    # check_exports: Check if 'symbol' is exported from 'namespace' in
    #           'namespace::symbol' calls.
    #
    # check_nonexports: Check if 'symbol' exists in 'namespace' in
    #           'namespace:::symbol' calls.
    #
    namespace_linter(check_exports = TRUE, check_nonexports = TRUE),

    # Description:
    #
    #      Calling 'ifelse()' in nested calls is problematic for two main
    #      reasons:
    #
    #        1. It can be hard to read -- mapping the code to the expected
    #           output for such code can be a messy task/require a lot of
    #           mental bandwidth, especially for code that nests more than
    #           once
    #
    #        2. It is inefficient -- 'ifelse()' can evaluate _all_ of its
    #           arguments at both yes and no (see
    #           <https://stackoverflow.com/q/16275149>); this issue is
    #           exacerbated for nested calls
    #
    nested_ifelse_linter(),

    # Description:
    #
    #      Check that 'file.path()' is used to construct safe and portable
    #      paths.
    #
    # Arguments:
    #
    #      lax: Less stringent linting, leading to fewer false positives. If
    #           'TRUE', only lint path strings, which
    #
    #             • contain at least two path elements, with one having at
    #               least two characters and
    #
    #             • contain only alphanumeric chars (including UTF-8),
    #               spaces, and win32-allowed punctuation
    #
    #nonportable_path_linter(lax = TRUE),

    # Description:
    #
    #      While .1 and 0.1 mean the same thing, the latter is easier to read
    #      due to the small size of the '.' glyph.
    #
    numeric_leading_zero_linter(),

    # Description:
    #
    #      Check that object names are not too long. The length of an object
    #      name is defined as the length in characters, after removing
    #      extraneous parts:
    #
    # Arguments:
    #
    #   length: maximum variable name length allowed.
    #
    #object_length_linter(length = 30L),

    # Description:
    #
    #      Check that object names conform to a naming style. The default
    #      naming styles are "snake_case" and "symbols".
    #
    # Arguments:
    #
    #   styles: A subset of \Sexpr[stage=render,
    #           results=rd]{lintr:::regexes_rd}. A name should match at least
    #           one of these styles. The '"symbols"' style refers to names
    #           containing _only_ non-alphanumeric characters; e.g., defining
    #           %+% from ggplot2 or %>% from magrittr would not generate lint
    #           markers, whereas %m+% from lubridate (containing both
    #           alphanumeric _and_ non-alphanumeric characters) would.
    #
    #  regexes: A (possibly named) character vector specifying a custom
    #           naming convention. If named, the names will be used in the
    #           lint message. Otherwise, the regexes enclosed by '/' will be
    #           used in the lint message. Note that specifying 'regexes'
    #           overrides the default 'styles'. So if you want to combine
    #           'regexes' and 'styles', both need to be explicitly specified.
    #
    #object_name_linter(styles = c("snake_case", "symbols"), regexes = character()),

    # Description:
    #
    #      Check that closures have the proper usage using
    #      'codetools::checkUsage()'. Note that this runs 'base::eval()' on
    #      the code, so *do not use with untrusted code*.
    #
    # Arguments:
    #
    # interpret_glue: If 'TRUE', interpret 'glue::glue()' calls to avoid
    #           false positives caused by local variables which are only used
    #           in a glue expression.
    #
    # skip_with: A logical. If 'TRUE' (default), code in 'with()' expressions
    #           will be skipped. This argument will be passed to 'skipWith'
    #           argument of 'codetools::checkUsage()'.
    #
    #object_usage_linter(interpret_glue = TRUE, skip_with = TRUE),

    # Description:
    #
    #      'any(!x)' is logically equivalent to '!any(x)'; ditto for the
    #      equivalence of 'all(!x)' and '!any(x)'. Negating after aggregation
    #      only requires inverting one logical value, and is typically more
    #      readable.
    #
    outer_negation_linter(),

    # Description:
    #
    #      Check various common "gotchas" in '.onLoad()', '.onAttach()',
    #      '.Last.lib()', and '.onDetach()' namespace hooks that will cause R
    #      CMD check issues. See Writing R Extensions for details.
    #
    package_hooks_linter(),

    # Description:
    #
    #      Check that there is a space between right parenthesis and a body
    #      expression.
    #
    paren_body_linter(),

    # Description:
    #
    #      The following issues are linted by default by this linter (see
    #      arguments for which can be de-activated optionally):
    #
    # Arguments:
    #
    # allow_empty_sep: Logical, default 'FALSE'. If 'TRUE', usage of
    #           'paste()' with 'sep = ""' is not linted.
    #
    # allow_to_string: Logical, default 'FALSE'. If 'TRUE', usage of
    #           'paste()' and 'paste0()' with 'collapse = ", "' is not
    #           linted.
    #
    # allow_file_path: String, one of '"never"', '"double_slash"', or
    #           '"always"'; '"double_slash"' by default. If '"never"', usage
    #           of 'paste()' and 'paste0()' to construct file paths is not
    #           linted. If '"double_slash"', strings containing consecutive
    #           forward slashes will not lint. The main use case here is for
    #           URLs - "paths" like '"https://"' will not induce lints, since
    #           constructing them with 'file.path()' might be deemed
    #           unnatural. Lastly, if '"always"', strings with consecutive
    #           forward slashes will also lint. Note that '"//"' is never
    #           linted when it comes at the beginning or end of the input, to
    #           avoid requiring empty inputs like 'file.path("", ...)' or
    #           'file.path(..., "")'.
    #
    paste_linter(allow_empty_sep = FALSE, allow_to_string = FALSE, allow_file_path = c("double_slash", "always", "never")),

    # Description:
    #
    #      Force explicit calls in magrittr pipes, e.g., '1:3 %>% sum()'
    #      instead of '1:3 %>% sum'. Note that native pipe always requires a
    #      function call, i.e. 1:3 |> sum will produce an error.
    #
    pipe_call_linter(),

    # Description:
    #
    #      Check that pipe operators are used consistently by file, or
    #      optionally specify one valid pipe operator.
    #
    # Arguments:
    #
    #     pipe: Which pipe operator is valid (either '"%>%"' or '"|>"'). By
    #           default ('"auto"'), the linter has no preference but will
    #           check that each file uses only one type of pipe operator.
    #
    pipe_consistency_linter(pipe = c("auto", "%>%", "|>")),

    # Description:
    #
    #      Check that each step in a pipeline is on a new line, or the entire
    #      pipe fits on one line.
    #
    pipe_continuation_linter(),

    # Description:
    #
    #      Check that the desired quote delimiter is used for string
    #      constants.
    #
    # Arguments:
    #
    # delimiter: Which quote delimiter to accept. Defaults to the tidyverse
    #           default of " (double-quoted strings).
    #
    quotes_linter(delimiter = c("\"", "'")),

    # Description:
    #
    #      Testing 'x == TRUE' is redundant if 'x' is a logical vector.
    #      Wherever this is used to improve readability, the solution should
    #      instead be to improve the naming of the object to better indicate
    #      that its contents are logical. This can be done using prefixes
    #      (is, has, can, etc.). For example, 'is_child',
    #      'has_parent_supervision', 'can_watch_horror_movie' clarify their
    #      logical nature, while 'child', 'parent_supervision',
    #      'watch_horror_movie' don't.
    #
    redundant_equals_linter(),

    # Description:
    #
    #      Expressions like 'ifelse(x, TRUE, FALSE)' and 'ifelse(x, FALSE,
    #      TRUE)' are redundant; just 'x' or '!x' suffice in R code where
    #      logical vectors are a core data structure. 'ifelse(x, 1, 0)' is
    #      also 'as.numeric(x)', but even this should be needed only rarely.
    #
    # Arguments:
    #
    #  allow10: Logical, default 'FALSE'. If 'TRUE', usage like 'ifelse(x, 1,
    #           0)' is allowed, i.e., only usage like 'ifelse(x, TRUE,
    #           FALSE)' is linted.
    #
    redundant_ifelse_linter(allow10 = FALSE),

    # Description:
    #
    #      Using 'value = TRUE' in 'grep()' returns the subset of the input
    #      that matches the pattern, e.g. 'grep("[a-m]", letters, value =
    #      TRUE)' will return the first 13 elements ('a' through 'm').
    #
    regex_subset_linter(),

    # Description:
    #
    #      Check that while (TRUE) is not used for infinite loops.
    #
    repeat_linter(),

    # Description:
    #
    #      It is preferable to register routines for efficiency and safety.
    #
    routine_registration_linter(),

    # Description:
    #
    #      'vector %in% set' is appropriate for matching a vector to a set,
    #      but if that set has size 1, '==' is more appropriate. %chin% from
    #      '{data.table}' is matched as well.
    #
    scalar_in_linter(),

    # Description:
    #
    #      Check that no semicolons terminate expressions.
    #
    # Arguments:
    #
    # allow_compound: Logical, default 'FALSE'. If 'TRUE', "compound"
    #           semicolons (e.g. as in x; y, i.e., on the same line of code)
    #           are allowed.
    #
    # allow_trailing: Logical, default 'FALSE'. If 'TRUE', "trailing"
    #           semicolons (i.e., those that terminate lines of code) are
    #           allowed.
    #
    semicolon_linter(allow_compound = FALSE, allow_trailing = FALSE),

    # Description:
    #
    #      This linter checks for '1:length(...)', '1:nrow(...)',
    #      '1:ncol(...)', '1:NROW(...)' and '1:NCOL(...)' expressions in
    #      base-R, or their usage in conjunction with 'seq()' (e.g.,
    #      'seq(length(...))', 'seq(nrow(...))', etc.).
    #
    seq_linter(),

    # Description:
    #
    #      This linter checks for some common mistakes when using 'order()'
    #      or 'sort()'.
    #
    sort_linter(),

    # Description:
    #
    #      Check that parentheses and square brackets do not have spaces
    #      directly inside them, i.e., directly following an opening
    #      delimiter or directly preceding a closing delimiter.
    #
    spaces_inside_linter(),

    # Description:
    #
    #      Check that all left parentheses have a space before them unless
    #      they are in a function call.
    #
    spaces_left_parentheses_linter(),

    # Description:
    #
    #      Check for an inconsistent number of arguments or arguments with
    #      incompatible types (for literal arguments) in 'sprintf()' calls.
    #
    sprintf_linter(),

    # Description:
    #
    #      'startsWith()' is used to detect fixed initial substrings; it is
    #      more readable and more efficient than equivalents using 'grepl()'
    #      or 'substr()'. c.f. 'startsWith(x, "abc")', 'grepl("^abc", x)',
    #      'substr(x, 1L, 3L) == "abc"'.
    #
    # Arguments:
    #
    # allow_grepl: Logical, default 'FALSE'. If 'TRUE', usages with 'grepl()'
    #           are ignored. Some authors may prefer the conciseness offered
    #           by 'grepl()' whereby 'NA' input maps to 'FALSE' output, which
    #           doesn't have a direct equivalent with 'startsWith()' or
    #           'endsWith()'.
    #
    string_boundary_linter(allow_grepl = FALSE),

    # Description:
    #
    #      Designed for code bases written for versions of R before 4.0
    #      seeking to upgrade to R >= 4.0, where one of the biggest pain
    #      points will surely be the flipping of the default value of
    #      'stringsAsFactors' from 'TRUE' to 'FALSE'.
    #
    strings_as_factors_linter(),

    # Description:
    #
    #      'system.file()' has a '...' argument which, internally, is passed
    #      to 'file.path()', so including it in user code is repetitive.
    #
    system_file_linter(),

    # Description:
    #
    #      Avoid the symbols 'T' and 'F', and use 'TRUE' and 'FALSE' instead.
    #
    T_and_F_symbol_linter(),

    # Description:
    #
    #      Check that the source contains no TODO comments
    #      (case-insensitive).
    #
    # Arguments:
    #
    #     todo: Vector of strings that identify TODO comments.
    #
    todo_comment_linter(todo = c("todo", "fixme")),

    # Description:
    #
    #      Check that there are no trailing blank lines in source code.
    #
    trailing_blank_lines_linter(),

    # Description:
    #
    #      Check that there are no space characters at the end of source
    #      lines.
    #
    # Arguments:
    #
    # allow_empty_lines: Suppress lints for lines that contain only
    #           whitespace.
    #
    # allow_in_strings: Suppress lints for trailing whitespace in string
    #           constants.
    #
    trailing_whitespace_linter(allow_empty_lines = FALSE, allow_in_strings = TRUE),

    # Description:
    #
    #      Report the use of undesirable functions (e.g. 'base::return()',
    #      'base::options()', or 'base::sapply()') and suggest an
    #      alternative.
    #
    # Arguments:
    #
    #      fun: Named character vector. 'names(fun)' correspond to
    #           undesirable functions, while the values give a description of
    #           why the function is undesirable. If 'NA', no additional
    #           information is given in the lint message. Defaults to
    #           default_undesirable_functions. To make small customizations
    #           to this list, use 'modify_defaults()'.
    #
    # symbol_is_undesirable: Whether to consider the use of an undesirable
    #           function name as a symbol undesirable or not.
    #
    undesirable_function_linter(fun = default_undesirable_functions, symbol_is_undesirable = TRUE),

    # Description:
    #
    #      Report the use of undesirable operators, e.g. ':::' or '<<-' and
    #      suggest an alternative.
    #
    # Arguments:
    #
    #       op: Named character vector. 'names(op)' correspond to undesirable
    #           operators, while the values give a description of why the
    #           operator is undesirable. If 'NA', no additional information
    #           is given in the lint message. Defaults to
    #           default_undesirable_operators. To make small customizations
    #           to this list, use 'modify_defaults()'.
    #
    undesirable_operator_linter(op = default_undesirable_operators),

    # Description:
    #
    #      Check that the 'c()' function is not used without arguments nor
    #      with a single constant.
    #
    # Arguments:
    #
    # allow_single_expression: Logical, default 'TRUE'. If 'FALSE',
    #           one-expression usages of 'c()' are always linted, e.g. 'c(x)'
    #           and 'c(matrix(...))'. In some such cases, 'c()' is being used
    #           for its side-effect of stripping non-name attributes; it is
    #           usually preferable to use the more readable 'as.vector()'
    #           instead. 'as.vector()' is not always preferable, for example
    #           with environments (especially, 'R6' objects), in which case
    #           'list()' is the better alternative.
    #
    unnecessary_concatenation_linter(allow_single_expression = TRUE),

    # Description:
    #
    #      Using an anonymous function in, e.g., 'lapply()' is not always
    #      necessary, e.g. 'lapply(DF, sum)' is the same as 'lapply(DF,
    #      function(x) sum(x))' and the former is more readable.
    #
    unnecessary_lambda_linter(),

    # Description:
    #
    #      Avoid unnecessary nested 'if' conditional statements
    #
    unnecessary_nested_if_linter(),

    # Description:
    #
    #      The argument placeholder '.' in magrittr pipelines is unnecessary
    #      if passed as the first positional argument; using it can cause
    #      confusion and impacts readability.
    #
    unnecessary_placeholder_linter(),

    # Description:
    #
    #      Code after e.g. a 'return()' or 'stop()' or in deterministically
    #      false conditional loops like if (FALSE) can't be reached;
    #      typically this is vestigial code left after refactoring or
    #      sandboxing code, which is fine for exploration, but shouldn't
    #      ultimately be checked in. Comments meant for posterity should be
    #      placed _before_ the final 'return()'.
    #
    unreachable_code_linter(),

    # Description:
    #
    #      Check that imported packages are actually used
    #
    # Arguments:
    #
    # allow_ns_usage: Suppress lints for packages only used via namespace.
    #           This is 'FALSE' by default because 'pkg::fun()' doesn't
    #           require 'library(pkg)'. You can use requireNamespace("pkg")
    #           to ensure a package is installed without loading it.
    #
    # except_packages: Character vector of packages that are ignored. These
    #           are usually attached for their side effects.
    #
    # interpret_glue: If 'TRUE', interpret 'glue::glue()' calls to avoid
    #           false positives caused by local variables which are only used
    #           in a glue expression.
    #
    unused_import_linter(allow_ns_usage = FALSE, except_packages = c("bit64", "data.table", "tidyverse"), interpret_glue = TRUE),

    # Description:
    #
    #      Usage of '&' in conditional statements is error-prone and
    #      inefficient. 'condition' in 'if (condition) expr' must always be
    #      of length 1, in which case '&&' is to be preferred. Ditto for '|'
    #      vs. '||'.
    #
    vector_logic_linter(),

    # Description:
    #
    #      Check that the correct character is used for indentation.
    #
    whitespace_linter(),

    # Description:
    #
    #      Yoda tests use (expected, actual) instead of the more common
    #      (actual, expected). This is not always possible to detect
    #      statically; this linter focuses on the simple case of testing an
    #      expression against a literal value, e.g. (1L, foo(x)) should be
    #      (foo(x), 1L).
    #
    yoda_test_linter()
)
